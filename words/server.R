library(shiny)
library(datasets)
library(plyr)
word.freqs <- read.csv("wordfreqs.csv",sep=",")
word.freqs$date <- as.Date(word.freqs$date)
word.freqs$term <- as.factor(word.freqs$term)
word.freqs$frequency <- as.numeric(word.freqs$frequency)

speeches <- read.csv("speeches.csv",sep=",")
colnames(speeches) <- c("date","time","speaker","text")
speeches$date <- as.character(speeches$date)
speeches$time <- as.numeric(speeches$time)
speeches$speaker <- as.factor(speeches$speaker)
speeches$text <- as.character(speeches$text)


shinyServer(function(input, output) {

	output$wordPlot <- reactivePlot(function() {
		
	word <- subset(word.freqs, term==input$term)
	word <- ddply(word, .(date), numcolwise(sum))
	plot(frequency ~ date, type="h", col=2, data=word)

  })
    
   output$view <- reactiveTable(function() {
   	word <- subset(word.freqs, term==input$term)
	word <- ddply(word, .(date), numcolwise(sum))
    
    speech.output <- as.data.frame(matrix(data=NA, nrow=0, ncol=4))
    k <- 0
    for(j in 1:dim(word)[1]) {
    	w <- word[j,]
    	
    	speech.date <- which(speeches$date==as.character(w$date))
	    for(i in speech.date) {
	    	k <- k+1
	    	if(length(which(strsplit(speeches[i,]$text," ")[[1]]==input$term))!=0)
	    		{speech.output <- rbind(speech.output, speeches[i,])}	
	    }
	}
	    
    speech.output
  })
  
})