#read source file
data <- read.csv(file='SP500.csv')
head(data, 10)
colnames(data)
data <- data[7:nrow(data),7:length(data)]
stocksdb1 <- stocksdb
stocksdb <- data[,2:length(data)]
#creating a new column
for (i in 1:nrow(stocksdb))
{
  stocksdb$PriceDirection[i] <- ifelse(data$Adj.Close[i+1] > data$Adj.Close[i],1,0)
}

#creating test and training set as per instructions #sampling data
rnum <- (runif(1, .60, .70))
rnum
#rnum <- as.integer(format(num, digits = 2))
part <-sample(1:nrow(stocksdb), rnum * nrow(stocksdb))
trng.d <- stocksdb[part,]
test.d <- stocksdb[-part,]

#Logression regression model
lr.stocks <- glm(PriceDirection ~ ., family=binomial(link="logit"), data=trng.d)
summary(lr.stocks)

#lets predict
pred <- predict(lr.stocks,newdata=test.d, type="response")
head(pred)
#converting predictions > 50% to 1 and remaining to 0
pred_10 <- ifelse(pred > 0.5,1,0)
head(pred_10)

#table - confusion matrix
table(test.d$PriceDirection ,pred_10)
misClasificError <- mean(test.d$PriceDirection != t1[,])
t1<-as.data.frame(test.d$PriceDirection != pred_10)
misClasificError1 <- ifelse(t1[,] == 'TRUE',1,0)
print(paste('Accuracy', 1-misClasificError))

t1

