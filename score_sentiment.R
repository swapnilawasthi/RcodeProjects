score.sentiment = function(sample, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sample, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sample)
  positive.df = data.frame(Positive = pp1, text=sample)
  negative.df = data.frame(Negative = nn1, text=sample)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

#Clean tweets and return merged data frame
result <- score.sentiment(sample, pos.words , neg.words)

#creating copy of result data frame
test1 <- result[[1]]
test2 <- result[[2]]
test3 <- result[[3]]

#removing text column since it is redundant
test1$text <- NULL
test2$text <- NULL
test3$text <- NULL

#storing scores separtely
q1 <- test1[1,]
q2 <- test2[1,]
q3 <- test3[1,]

#using reshape lib to melt 
install.packages('reshape')
library('reshape')
qq1 <- melt(q1, , var='Score')
qq2 <- melt(q2, , var='Positive')
qq3 <- melt(q3, , var='Negative')

#no need of 1st column - removing
qq1[1] <- NULL
qq2[1] <- NULL
qq3[1] <- NULL

table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#Histogram
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
library(plotrix)
#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")
