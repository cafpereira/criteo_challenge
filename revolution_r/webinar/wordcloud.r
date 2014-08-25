
#------------------------------------------------------------ 
# REVOLUTION ANALYTICS WEBINAR: INTRODUCTION TO R FOR DATA MINING
# February 14, 2013
# Joseph B. Rickert
# Technical Marketing Manager
#
# JUST FOR FUN - BUILD A WORD CLOUD
#
# Copyright: Revolution Analytics
# This script is licensed under the GPLv2 license
# http://www.gnu.org/licenses/gpl-2.0.html


# From example at RDataMining
# http://www.rdatamining.com/examples/text-mining
# This page shows an example on text mining of Twitter 
#-----------------------------------------------------------------------------
# Load the libraries necesary
library(twitteR)			# twitteR provides access to Twitter data
library(tm)					# tm provides functions for text mining
library(Snowball)           # Wrappers for Weka Java stemming funcitons
library(wordcloud)          # wordcloud visualizes the result with a word cloud
library(RColorBrewer)       # provides the rainbow colors
#------------------------------------------------------------------------------
# retrieve the first 100 tweets (or all tweets if fewer than 100)
# from the user timeline of @rdatammining
#
Tweets <- searchTwitter("#rstats",n=100)
n <- length(Tweets)
# Tweets[1:3]
#
#-------------------------------------------------------------------------------
#Transforming Text
#The tweets are first converted to a data frame and then to a corpus.
df <- do.call("rbind", lapply(Tweets, as.data.frame))
#dim(df)
# Just in case twitter is off-line
#df <-read.csv("UseRTweets.csv",header=TRUE,row.names=1)
#head(df)
#
# Build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(df$text))

#After that, the corpus needs a couple of transformations, including 
   #changing letters to lower case, 
   #removing punctuations/numbers and removing stop words. 
#The general English stop-word list is tailored by 
#adding "available" and "via" and removing "r".

myCorpus <- tm_map(myCorpus, tolower)					# lower case
myCorpus <- tm_map(myCorpus, removePunctuation)			# remove punctuation
myCorpus <- tm_map(myCorpus, removeNumbers)				# remove numbers
# keep "r" by removing it from stopwords
myStopwords <- c(stopwords('english'), "available", "via")
idx <- which(myStopwords == "r")
myStopwords <- myStopwords[-idx]
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
#----------------------------------------------------------------------------
#Stemming Words
#  In many cases, words need to be stemmed to retrieve their radicals. 
#  For instance, "example" and "examples" are both stemmed to "exampl". 
#  However, after that, one may want to complete the stems to their original 
#  forms, so that the words would look "normal".

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
myCorpus <- tm_map(myCorpus, stemDocument)

#inspect(myCorpus[1:3])   # inspect the first three ``documents"
#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus) # stem completion
#
#
#inspect(myCorpus[1:3])   #Print the first three documents in the built corpus.
#----------------------------------------------------------------------------------------
#Building a Document-Term Matrix
myDtm <- TermDocumentMatrix(myCorpus, control = list(minWordLength = 1))
# inspect(myDtm[266:270,31:40])
#
# findFreqTerms(myDtm, lowfreq=10)		#Frequent Terms and Associations
# findAssocs(myDtm, 'analytics', 0.30)    # which words are associated with "analytics"?
#-----------------------------------------------------------------------------------------
#Build the word cloud
#After building a document-term matrix, we can show the importance of 
#words with a word cloud (also kown as a tag cloud) . 
m <- as.matrix(myDtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
# Plot the word cloud
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]	
#random colors
wordcloud(d$word,d$freq,c(4,1),2,,TRUE,TRUE,.15,pal)


	
