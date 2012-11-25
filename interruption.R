library(tm)
library(ggplot2)
library(XML)
library(wordcloud)
library(plyr)

setwd("/Users/john/Dropbox/rsparly2012")


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

interruption <- numeric(dim(s)[1])
for(i in 1:dim(s)[1]) {
	if(i%%100==0) {print(i)}
	s.tdm <- get.tdm(s$text[i])
	speech.matrix <- as.matrix(s.tdm)
	speech.counts <- rowSums(speech.matrix)
	speech.df <- data.frame(cbind(names(speech.counts), as.numeric(speech.counts)), stringsAsFactors=FALSE)
	names(speech.df) <- c("term", "frequency")
	speech.df$frequency <- as.numeric(speech.df$frequency)
	speech.df$term <- as.factor(speech.df$term)
	if(length(which(speech.df$term=="interruption"))==0) {
		interruption[i] <- 0
	}
	else { interruption[i] <- 1}
}

s <- cbind(s,interruption)

s.rowdy <- subset(s, interruption==1)
s.calm <- subset(s, interruption==0)
s.rowdy <- s.rowdy[,c(1,2,3,4)]
s.calm <- s.calm[,c(1,2,3,4)]

calm.tdm.0 <- get.tdm(s.calm$text)
rowdy.tdm <- get.tdm(s.rowdy$text)

calm.counts <- numeric()
for(i in 1:82) {
	print(i)
	calm.tdm <- get.tdm(s.calm$text[((i-1)*1000+1):(i*1000)])
	calm.matrix <- as.matrix(calm.tdm)
	calm.counts.temp <- rowSums(calm.matrix)
	calm.counts <- c(calm.counts,calm.counts.temp)
}

calm.tdm <- get.tdm(s.calm$text[82001:length(s.calm$text)])
calm.matrix <- as.matrix(calm.tdm)
calm.counts.temp <- rowSums(calm.matrix)
calm.counts <- c(calm.counts,calm.counts.temp)

rowdy.matrix <- as.matrix(rowdy.tdm)
rowdy.counts <- rowSums(rowdy.matrix)

calm.df <- data.frame(cbind(names(calm.counts), as.numeric(calm.counts)), stringsAsFactors=FALSE)
rowdy.df <- data.frame(cbind(names(rowdy.counts), as.numeric(rowdy.counts)), stringsAsFactors=FALSE)

names(calm.df) <- c("term","frequency")
names(rowdy.df) <- c("term","frequency")

calm.df$frequency <- as.numeric(calm.df$frequency)
rowdy.df$frequency <- as.numeric(rowdy.df$frequency)

test <- ddply(calm.df, .(term), numcolwise(sum))
calm.df <- test

calm.occurrence <- calm.df$frequency / 78505
rowdy.occurrence <- sapply(1:nrow(rowdy.matrix), function(i) { length(which(rowdy.matrix[i, ] > 0)) / ncol(rowdy.matrix)})

calm.density <- calm.df$frequency / sum(calm.df$frequency)
rowdy.density <- rowdy.df$frequency / sum(rowdy.df$frequency)

calm.df <- transform(calm.df, density=calm.density, occurrence=calm.occurrence)
rowdy.df <- transform(rowdy.df, density=rowdy.density, occurrence=rowdy.occurrence)

head(calm.df[with(calm.df, order(-frequency)),])
head(rowdy.df[with(rowdy.df, order(-frequency)),])


#classify.text <- function(path, training.df, prior=0.5, c=1e-6) {
#	msg <- get.msg(path)
#	msg.tdm <- get.tdm(msg)
#	msg.freq <- rowSums(as.matrix(msg.tdm))
#	msg.match <- intersect(names(msg.freq), training.df$term)
#	if(length(msg.match) < 1) {
#		return(prior*c^(length(msg.freq)))
#	}
#	else {
#		match.probs <- training.df$occurrence[match(msg.match, training.df$term)]
#		return(prior * prod(match.probs) * c^(length(msg.freq)-length(msg.match)))
#	}
#}

rd <- rowdy.df[with(rowdy.df, order(-frequency)),][1:200,]
cd <- calm.df[with(calm.df, order(-frequency)),][1:200,]

ss.r <- setdiff(rd$term, cd$term)
ss.r <- ss.r[-1]
ss.c <- setdiff(cd$term, rd$term)
ss <- c(ss.r,ss.c)


predictors <- as.data.frame(matrix(data=NA, nrow=0, ncol=3))
colnames(predictors) <- c("term","frequency","type")
for(i in 1:length(ss)) {
	if(length(which(rd$term==ss[i]))>0) {
		loc <- which(rd$term==ss[i])
		term <- ss[i]
		freq <- rd$frequency[loc]
		type <- "red"
	} else {
		loc <- which(cd$term==ss[i])
		term <- ss[i]
		freq <- cd$frequency[loc]/40
		type <- "blue"
	}
	predictors[i,] <- c(term, freq, type)
}
predictors$frequency <- as.numeric(predictors$frequency)

wordcloud(predictors$term, predictors$frequency, c(4,.2), color=predictors$type, ordered.colors=TRUE, random.color=FALSE, vfont=c("sans serif","plain"))


