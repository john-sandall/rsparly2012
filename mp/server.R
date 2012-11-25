library(shiny)
library(datasets)
library(tm)
library(ggplot2)
library(XML)
library(wordcloud)

s <- read.csv("speeches.csv",sep=",")
colnames(s) <- c("date","time","speaker","text")
s$date <- as.character(s$date)
s$time <- as.numeric(s$time)
s$speaker <- as.factor(s$speaker)
s$text <- as.character(s$text)

get.tdm <- function(doc.vec) {
	doc.corpus <- Corpus(VectorSource(doc.vec))
	control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
	doc.dtm <- TermDocumentMatrix(doc.corpus, control)
	return(doc.dtm)
}

shinyServer(function(input, output) {

	output$wordPlot <- reactivePlot(function() {
	
	s.mp <- subset(s, speaker==input$mp)
	
	word.freqs <- as.data.frame(matrix(data=NA, nrow=0, ncol=3))
colnames(word.freqs) <- c("date","term","frequency")

s.mp.tdm <- get.tdm(s.mp$text)
speech.matrix <- as.matrix(s.mp.tdm)
speech.counts <- rowSums(speech.matrix)
speech.df <- data.frame(cbind(names(speech.counts), as.numeric(speech.counts)), stringsAsFactors=FALSE)
names(speech.df) <- c("term", "frequency")
speech.df$frequency <- as.numeric(speech.df$frequency)
speech.df$term <- as.factor(speech.df$term)


wordcloud(speech.df$term, speech.df$frequency, min.freq=5, scale=c(12,0.2))

	

  })
  
})