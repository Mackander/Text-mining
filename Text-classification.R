# init

libs <- c("tm", "plyr", "class")
lapply(libs, require, character.only = TRUE)

# set options
options(stringsAsFactors = FALSE) # we don't want to convert text to string

# Set parameters
candidates <- c("pasta","pizza")
pathname <- "C:/ML/Text-mining/abstract text"

# clean text

# corpus is a collection of documents, so we create each corpus for each of the candidate
cleanCorpus <- function(corpus){
  corpus.tmp <- tm_map(corpus, removePunctuation)
  corpus.tmp <- tm_map(corpus.tmp, stripWhitespace)
  corpus.tmp <- tm_map(corpus.tmp, tolower)
  # and we remove part of speach that won't make so much meaning as they don't help us to identify the right candidate
  corpus.tmp <- tm_map(corpus.tmp, removeWords,stopwords("english")) 
  return (corpus.tmp)
}


# build Term Document Matrix (TDM)

generateTDM <- function(cand, path){
  s.dir <- sprintf("%s/%s", path,cand)
  s.cor <- Corpus(DirSource(directory = s.dir, encoding = "UTF8"))
  s.cor.cl <- cleanCorpus(s.cor)
  s.tdm <- TermDocumentMatrix(s.cor.cl)
  
  s.tdm <- removeSparseTerms(s.tdm, 0.7)
  result <- list(name = cand,tdm = s.tdm)
}

tdm <- lapply(candidates, generateTDM, path = pathname)

# attach name

bindCandidateToTDM <- function(tdm){
#I want to convert TDM to numeric datamatrix and also transpose row and column variables as each text as row and each term as column
  s.mat <- t(data.matrix(tdm[["tdm"]]))  
  s.df <- as.data.frame(s.mat, stringsAsFactors = FALSE)
  
  # Now at this point we have a data frame, think it as a form of spreadsheet, where each text is in row
  # and each term as column and each cell containing values that denotes the frequency of that term 
  # that appears in the text.
  
  # Now I want to put name of candidate to which text belongs to by using column bind
  s.df <- cbind(s.df,rep(tdm[["name"]],nrow(s.df)))
  colnames(s.df)[ncol(s.df)] <- "targetcandidate"
  return(s.df)
}

candTDM <- lapply(tdm, bindCandidateToTDM)


# stack

tdm.stack <- do.call(rbind.fill,candTDM)
tdm.stack[is.na(tdm.stack)] <- 0


# hold-out, train the model

train.idx <- sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * 0.7))
test.idx <- (1:nrow(tdm.stack)) [- train.idx]

# model - KNN

tdm.cand <- tdm.stack[,"targetcandidate"] # take the row with just target candidate
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% "targetcandidate"] # take all the columns except targetCandidate column

# Kenien next neighbour(knn) algorithm
knn.pred <- knn(tdm.stack.nl[train.idx,], tdm.stack.nl[test.idx, ], tdm.cand[train.idx])
#1. Learning it without the name of the candidate
#2. Testing it without the name of the candidate

#3. Giving the name of the candidates pasta,pizza


# Accuracy
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.cand[test.idx])
(accuracy <- sum(diag(conf.mat)) / length(test.idx) * 100)
