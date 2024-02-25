####################
# Exercise 3.1.3
####################

rm(list = ls()) # Clear work space

library(tm)
textfolder <- "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern = "textDocs*"), readerControl = list(language = "en")))

inspect(docs)
docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)

mystopwords <- scan("./Data/stopWords.txt", character(0))

docs_nostopwords <- tm_map(docs, removeWords, mystopwords)
inspect(docs_nostopwords)
dtm_nostopwords <- DocumentTermMatrix(docs_nostopwords, control = list(removePunctuation = TRUE, stopwords = TRUE))
inspect(dtm_nostopwords)

control <- list(stopwords = TRUE)
