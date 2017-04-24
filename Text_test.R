library('tm')
vdata <- read.csv(file = 'page_revision.csv', header = T, stringsAsFactors = F, check.names = T, na.strings = " " )
vdata.t <- vdata[,c('Added')]
vdata.df <- as.data.frame(vdata.t)
vdata.rmNA <- as.data.frame(vdata.t[complete.cases(vdata.t)]) #removing NAs
#changing name of column 
colnames(vdata.rmNA)[1] <- "Added"
#creating corpus 
corpus <- Corpus(DataframeSource(vdata.rmNA))
#getting all to lower case
corp_lower <- tm_map(corpus, tolower)
#coverting to plain text document since tolower coverts entire corpus to char format
corpus.strpwsp <- tm_map(corp_lower, PlainTextDocument)
#remove the numbers from the corpus
corp_nonums <- tm_map(corpus.strpwsp, removeNumbers)
#remove the punctuations from the corpus
corp_nopunt <- tm_map(corp_nonums, removePunctuation)
#remove the stopwords from the corpus
corpus.nostopw <- tm_map(corp_nopunt, removeWords, stopwords('english'))
#removing extra words - like word1 and word2
#corpus.nostopw <- tm_map(corpus.nostopw, removeWords, c("word1","word2"))
#strip whitespaces or blank spaces
corpus.strpwsp <- tm_map(corpus.nostopw , stripWhitespace)
#stem the document
corpus.strpwsp <- tm_map(corpus.strpwsp , stemDocument)
test <- corpus.strpwsp
dtmidf <- DocumentTermMatrix(test)
dtm.s <- removeSparseTerms(dtmidf, 0.99)
(inspect(dtm.s))
#plotting
term.freq <- rowSums(as.matrix(dtm.s))
term.freq <- subset(term.freq , term.freq >= 70)
library('ggplot2')
timeLinex <- c(-20 , +20)
timeLiney <- c(-20 , +20)
barplot(term.freq, xlim = timeLine, ylim = timeLiney )
#workcloud
install.packages('wordcloud')
library('wordcloud')
wordcloud(test, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))9

