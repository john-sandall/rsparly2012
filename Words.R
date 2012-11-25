library(tm)
library(ggplot2)
library(XML)

setwd("/Users/john/Dropbox/rsparly2012")

thefiles <- list.files("debates")

speeches.all <- as.data.frame(matrix(data=NA, nrow=0, ncol=4))
colnames(speeches) <- c("date","time","speaker","text")

word.freqs <- as.data.frame(matrix(data=NA, nrow=0, ncol=3))
colnames(word.freqs) <- c("date","term","frequency")
for(file in thefiles) {
	file.date <- as.Date(substr(file,8,17))
	print(file.date)
	
	doc = xmlTreeParse(paste("debates/",file,sep=""), useInternal=TRUE)
	top = xmlRoot(doc)
	nodes = getNodeSet(top, "//speech")

	if(length(nodes)>0) {
		
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
		speech.df$term <- as.factor(speech.df$term)
		
		speech.df <- cbind(file.date,speech.df)
		
		word.freqs <- rbind(word.freqs, speech.df)
		
		speeches.add <- cbind(as.character(file.date), as.character(speeches$time), as.character(speeches$speaker), as.character(speeches$text))
		speeches.all <- rbind(speeches.all, speeches.add)
		}
	}
	colnames(word.freqs) <- c("date","term","frequency")

	write.table(speeches.all,file="speeches.csv",sep=",",row.names=F)
	write.table(word.freqs,file="wordfreqs.csv",sep=",",row.names=F)

	which(word.freqs$term=="word")
	
	hon <- subset(word.freqs, term=="tax")
	hon <- ddply(hon, .(date), numcolwise(sum))
	plot(frequency ~ date, type="h", col=2, data=hon)
	
	

	

	