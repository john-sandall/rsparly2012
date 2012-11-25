library(tm)
library(ggplot2)

setwd("/Users/john/Dropbox/rsparly2012")


doc = xmlTreeParse("debates.xml", useInternal=TRUE)
top = xmlRoot(doc)
nodes = getNodeSet(top, "//speech")

speech.text = xpathApply(top, "//speech", xmlValue)
speech.speaker = lapply(nodes, function(x) xmlGetAttr(x, "speakername"))
speech.time = lapply(nodes, function(x) xmlGetAttr(x, "time"))

speeches <- as.data.frame(matrix(data=NA, nrow=0, ncol=3))
colnames(speeches) <- c("time","speakername","text")
for(i in 1:length(nodes)) {
	if(is.null(speech.time[[i]])) { s.time <- NA } else { s.time <- speech.time[[i]] }
	if(is.null(speech.speaker[[i]])) { s.speaker <- NA } else { s.speaker <- speech.speaker[[i]] }
	if(is.null(speech.text[[i]])) { s.text <- NA } else { s.text <- speech.text[[i]] }
	
	Encoding(s.text) <- "latin1"
	s.text <- iconv(s.text, "latin1", "ASCII", sub="")
	
	s.time <- sapply(strsplit(s.time,":"), function(x) {
		x <- as.numeric(x)
		x[1]+x[2]/60
		}
	)	
	speeches[i,] <- c(s.time, s.speaker, s.text)
}
speeches$speaker <- as.factor(speeches$speaker)
speeches$time <- as.numeric(speeches$time)

words = lapply(speeches[5,3], strsplit, "[[:space:]]")

get.tdm <- function(doc.vec) {
	doc.corpus <- Corpus(VectorSource(doc.vec))
	control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
	doc.dtm <- TermDocumentMatrix(doc.corpus, control)
	return(doc.dtm)
}

speech.tdm <- get.tdm(speeches$text)
speech.matrix <- as.matrix(speech.tdm)
speech.counts <- rowSums(speech.matrix)
speech.df <- data.frame(cbind(names(speech.counts), as.numeric(speech.counts)), stringsAsFactors=FALSE)
names(speech.df) <- c("term", "frequency")
speech.df$frequency <- as.numeric(speech.df$frequency)


speech.df[order(speech.df$frequency, decreasing=T)[1:100],]

Chris Grayling
Andrew Lansley
Maria Miller
Edward Vaizey

chris <- subset(speeches, speaker=="Chris Grayling")
andrew <- subset(speeches, speaker=="Andrew Lansley")
maria <- subset(speeches, speaker=="Maria Miller")
edward <- subset(speeches, speaker=="Edward Vaizey")






