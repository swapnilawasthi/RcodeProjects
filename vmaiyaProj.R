library(randomForest)
library(caret)
library(rpart)
library(RColorBrewer)
library(rattle)

rawdata <- read.csv(file.choose())
head(rawdata)
class(rawdata$DATE_STRT)
rawdata$DATE_STRT <- as.Date(rawdata$DATE_STRT, "%m/%d/%Y")
rawdata$DATE_CMIT <- as.Date(rawdata$DATE_CMIT, "%m/%d/%y")
rawdata$DATE_ICOM <- as.Date(rawdata$DATE_ICOM, "%m/%d/%y")

#creating our target variable
rawdata$result <- ifelse(rawdata$DATE_ICOM > rawdata$DATE_CMIT, 0, 1)
rawdata$result <- as.factor(rawdata$result)
colSums(is.na(rawdata))
#checking which index has na in it
which(is.na(rawdata$result))

rawdata <- rawdata[,colSums(is.na(rawdata)) == 0]
sample <- createDataPartition(y= rawdata$result, p = 0.70, list = F)
trngset <- rawdata[sample,]
testset <- rawdata[-sample,]

model1 <- rpart(result ~ ., data=trngset, method="class")
prediction1 <- predict(model1, testset, type = "class")
confusionMatrix(testset$result, prediction1)
fancyRpartPlot(model1)

colSums(is.na(trngset))
#removing na from our trngset
trngset <- na.omit(trngset)
model2 <- randomForest(result ~ ., data=trngset, method="class")
prediction2 <- predict(model2, testset, type = "class")
confusionMatrix(testset$result, prediction2)
table(prediction2, testset$result)
#check variable importance
varImpPlot(model2)


