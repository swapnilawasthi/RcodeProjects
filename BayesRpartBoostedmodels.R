library ("klaR")
library ("caret")
library ("e1071")
set.seed(1234)
OptDigits = read.csv(file.choose())

OptDigits$OptDigit <- as.factor(OptDigits$OptDigit)
head(OptDigits)

#Clean out columns that are all "0"

ShuffledOptDigits <-OptDigits[sample(nrow(OptDigits)),]
train <- ShuffledOptDigits[1:2810,]
test <- ShuffledOptDigits[2811:5620,]
head(train)


# remove some columns.
train <- train[,-c(1,7,8,9,15,16,17,23,24,25,32,33,40,41,48,49,55,56,57,63,64)]
test <- test[,-c(1,7,8,9,15,16,17,23,24,25,32,33,40,41,48,49,55,56,57,63,64)]
model <- NaiveBayes(OptDigit ~ ., data=train)
#test the model
predictions <- predict(model, test)
warnings()
confusionMatrix(test$OptDigit, predictions$class)



RPTrain<-ShuffledOptDigits[1:2810,] 
RPTest<-ShuffledOptDigits[2811:5620,] 
library(ggplot2) 
library(rpart) 
OptDigit.ct<-rpart(RPTrain$OptDigit ~ ., data=RPTrain[,1:64], cp=0) 
plot(OptDigit.ct) 
text(OptDigit.ct, use.n = T, digits = 3, cex = 0.6)

Prediction <- predict(OptDigit.ct, newdata=RPTest, type='class')
head(Prediction)

table(Prediction, RPTest$OptDigit)


#Something is not quite right here.
library(gbm)
OptDigit.gbm <- gbm(OptDigit ~ ., data=RPTrain, n.trees=200, interaction.depth=6, shrinkage=0.01)
Prediction = predict(OptDigit.gbm, newdata=RPTest, n.trees=200, type="response")
Prediction[1,,]
passing <- 0.5
PredictionClass = ifelse(Prediction[,1,]>passing,0,0)
PredictionClass = ifelse(Prediction[,2,]>passing,1,PredictionClass)
PredictionClass = ifelse(Prediction[,3,]>passing,2,PredictionClass)
PredictionClass = ifelse(Prediction[,4,]>passing,3,PredictionClass)
PredictionClass = ifelse(Prediction[,5,]>passing,4,PredictionClass)
PredictionClass = ifelse(Prediction[,6,]>passing,5,PredictionClass)
PredictionClass = ifelse(Prediction[,7,]>passing,6,PredictionClass)
PredictionClass = ifelse(Prediction[,8,]>passing,7,PredictionClass)
PredictionClass = ifelse(Prediction[,9,]>passing,8,PredictionClass)
PredictionClass = ifelse(Prediction[,10,]>passing,9,PredictionClass)
PredictionClass

table(PredictionClass, RPTest$OptDigit)

