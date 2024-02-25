####################
# Exercise 3.1.4
####################

rm(list = ls()) # Clear work space

library(tm) # install.packages("tm")
library(SnowballC) # install.packages("SnowballC")

textfolder <- "./Data/"

(docs <- Corpus(DirSource(textfolder, pattern = "textDocs*"), readerControl = list(language = "en")))

inspect(docs)
docs_nopunct <- tm_map(docs, removePunctuation)
dtm <- DocumentTermMatrix(docs_nopunct)

mystopwords <- scan("./Data/stopWords.txt", character(0))

docs_nostopwords <- tm_map(docs_nopunct, removeWords, mystopwords)
dtm_nostopwords <- DocumentTermMatrix(docs_nostopwords, control = list(removePunctuation = TRUE, stopwords = TRUE))
inspect(dtm_nostopwords)

docs_stemmed <- tm_map(docs_nostopwords, stemDocument, language = "english")


inspect(docs_stemmed)
dtm_stemmed <- DocumentTermMatrix(docs_stemmed, control = list(stopwords = TRUE))
inspect(dtm_stemmed)
