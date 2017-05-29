library('caret')
library('rpart')
library('randomForest')

trngurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testurl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trng <- read.csv(trngurl, na.strings =c("NA","#DIV/0!", ""))
test <- read.csv(testurl, na.strings =c("NA","#DIV/0!", ""))

#removing the columns having NAs - since our dataset is very high dimensional
trngnonzero <- trng[,colSums(is.na(trng)) == 0]
testnonzero <- test[,colSums(is.na(test)) == 0]

#removing non relevant columns such as time stamps, dates, serial numbers etc

trngrelcols <- trngnonzero[,-c(1:7)]
testrelcols <- testnonzero[,-c(1:7)]

#data partitioning
sample <- createDataPartition(y= trngrelcols$classe, p = 0.70, list = F)
trngset <- trngrelcols[sample,]
testset <- trngrelcols[-sample,]

# recursive partition
model1 <- rpart(classe ~ ., data=trngset, method="class")
summary(model1)
prediction1 <- predict(model1, testset, type = "class")
confusionMatrix(testset$classe, prediction1)

# random forest
model2 <- randomForest(classe ~ ., data=trngset, method="class")
prediction2 <- predict(model2, testset, type = "class")
confusionMatrix(testset$classe, prediction2)

# reducing dimensionality of dataset by removing the less contributing columns
ycol <- trngset$classe
ycoltest <- testset$classe
nzv <- nearZeroVar(trngset, saveMetrics = TRUE)
print(paste('Range:',range(nzv$percentUnique)))
nzv

#sort
sort(nzv$percentUnique, decreasing = TRUE)

#selecting column having unique percent greater than 5
pcacols <- trngset[c(rownames(nzv[nzv$percentUnique > 5,]))]
pcacolstest <- testset[c(rownames(nzv[nzv$percentUnique > 5,]))]


pcacols$classe <- ycol
pcacolstest$classe <- ycoltest

pcamodel <- randomForest(classe ~ ., data=pcacols, method="class")
prediction3 <- predict(pcamodel, pcacolstest, type = "class")
confusionMatrix(pcacolstest$classe, prediction3)


## FINAL MODEL ##
testds <- testrelcols[c(rownames(nzv[nzv$percentUnique > 5,]))]
finalpred <- predict(pcamodel, testds, type = "class")
finalpred




