#load data
data <- read.csv(file.choose())

###############
# NAIVE BAYES##
###############
library ("klaR") 
library ("caret") 
library ("e1071")
set.seed(1234)

ShuffledData <-data[sample(nrow(data)),]
train <- ShuffledData[1:3231,]
test <- ShuffledData[3232:6462,]
head(train)

model <- NaiveBayes(Change_class ~ ., data = train)
prediction <- predict(model,test)
prediction
confusionMatrix(test$Change_class, prediction$class)


##############
#####Rpart####
##############
RPTrain<-ShuffledData[1:3231,] 
RPTest<-ShuffledData[3232:6462,]
library(ggplot2) 
library(rpart) 
OptDigit.ct<-rpart(RPTrain$Change_class ~ ., data=RPTrain[,1:16], cp=0) 
plot(OptDigit.ct) 
text(OptDigit.ct, use.n = T, digits = 3, cex = 0.6)
Prediction <- predict(OptDigit.ct, newdata=RPTest, type='class') 
head(Prediction)
tab <- table(Prediction, RPTest$Change_class)
tab


######
#gbm##
library(gbm) 
Data.gbm <- gbm(Change_class ~ ., data=RPTrain, n.trees=250, interaction.depth=6, shrinkage=0.01) 
Prediction = predict(Data.gbm, newdata=RPTest, n.trees=200, type="response") 
Prediction[1,,] 
head(RPTest)
passing = 0.3
PredictionClass = ifelse(Prediction[,1,]>passing,"Awful","Awful") 
PredictionClass = ifelse(Prediction[,2,]>passing,"Bad",PredictionClass) 
PredictionClass = ifelse(Prediction[,3,]>passing,"Good",PredictionClass) 
PredictionClass = ifelse(Prediction[,4,]>passing,"Great",PredictionClass) 
PredictionClass = ifelse(Prediction[,5,]>passing,"Unchanged",PredictionClass) 
#PredictionClass = ifelse(Prediction[,6,]>.5,5,PredictionClass) 
PredictionClass
table(PredictionClass, RPTest$Change_class)


####
#Confusion matrix#
###

#rpart#
control <- trainControl(method = "cv", number = 10 , savePredictions = TRUE)
price.rpt.cv <- train (Change_class ~ ., data = RPTrain , method = "rpart", trControl = control)
pred.price.rpt.cv <- predict(price.rpt.cv, newdata = test, na.action = na.pass )
pred.price.rpt.cv
x <- test$Change_class
confusionMatrix(pred.price.rpt.cv , x)

#gbm#
control <- trainControl(method = "cv", number = 10 , savePredictions = TRUE)
price.rpt.cv <- train (Change_class ~ ., data = RPTrain , method = "gbm", trControl = control)
pred.price.rpt.cv <- predict(price.rpt.cv, newdata = test, na.action = na.pass )
pred.price.rpt.cv
x <- test$Change_class
confusionMatrix(pred.price.rpt.cv , x)
