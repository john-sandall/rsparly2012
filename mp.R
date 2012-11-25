library(tm)
library(ggplot2)
library(XML)
library(wordcloud)

setwd("/Users/john/Dropbox/rsparly2012")


s <- read.csv("speeches.csv",sep=",")
colnames(s) <- c("date","time","speaker","text")
s$date <- as.character(s$date)
s$time <- as.numeric(s$time)
s$speaker <- as.factor(s$speaker)
s$text <- as.character(s$text)

mp <- "Paul Burstow"

s.mp <- subset(s, speaker==mp)


word.freqs <- as.data.frame(matrix(data=NA, nrow=0, ncol=3))
colnames(word.freqs) <- c("date","term","frequency")

get.tdm <- function(doc.vec) {
	doc.corpus <- Corpus(VectorSource(doc.vec))
	control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
	doc.dtm <- TermDocumentMatrix(doc.corpus, control)
	return(doc.dtm)
}

for(i in 1:dim(s.mp)[1]) {
	s.mp.tdm <- get.tdm(s.mp$text[i])
	speech.matrix <- as.matrix(s.mp.tdm)
	speech.counts <- rowSums(speech.matrix)
	speech.df <- data.frame(cbind(names(speech.counts), as.numeric(speech.counts)), stringsAsFactors=FALSE)
	names(speech.df) <- c("term", "frequency")
	speech.df$frequency <- as.numeric(speech.df$frequency)
	speech.df$term <- as.factor(speech.df$term)
	speech.df <- cbind(s.mp$date[i],speech.df)
	colnames(speech.df) <- c("date","term","frequency")
	word.freqs <- rbind(word.freqs, speech.df)
}	

s.mp.tdm <- get.tdm(s.mp$text)
speech.matrix <- as.matrix(s.mp.tdm)
speech.counts <- rowSums(speech.matrix)
speech.df <- data.frame(cbind(names(speech.counts), as.numeric(speech.counts)), stringsAsFactors=FALSE)
names(speech.df) <- c("term", "frequency")
speech.df$frequency <- as.numeric(speech.df$frequency)
speech.df$term <- as.factor(speech.df$term)


wordcloud(speech.df$term, speech.df$frequency, min.freq=4)

