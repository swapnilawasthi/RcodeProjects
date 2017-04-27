library ("klaR")
library ("caret")
library ("e1071")
set.seed(1234)
Letters = read.csv(file.choose())
#for classification - converting to factor
Letters$Letter <- as.factor(Letters$Letter)
head(Letters)

#Clean out columns that are all "0"

Shuffledletters <-Letters[sample(nrow(Letters)),]
#halfway <- nrow(Letters)/2
halfway <- 12000
train <- Shuffledletters[1:halfway,]
test <- Shuffledletters[12001:20000,]
head(train)


# remove some columns.
train <- train[,-c(1,7,8,9,15,16,17,23,24,25,32,33,40,41,48,49,55,56,57,63,64)]
test <- test[,-c(1,7,8,9,15,16,17,23,24,25,32,33,40,41,48,49,55,56,57,63,64)]
model <- NaiveBayes(Letter ~ ., data=train)
#test the model
predictions <- predict(model, test)
warnings()
confusionMatrix(test$Letter, predictions$class)
table(test$Letter, predictions$class)


RPtrain <- Shuffledletters[1:halfway,]
RPtest <- Shuffledletters[12001:20000,]
#RPTrain<-ShuffledOptDigits[1:2810,] 
#RPTest<-ShuffledOptDigits[2811:5620,]
library(ggplot2) 
library(rpart) 
letter.ct<-rpart( RPtrain$Letter~ ., data=RPtrain[,1:16], cp=0) 
plot(letter.ct) 
text(letter.ct, use.n = T, digits = 3, cex = 0.6)

Prediction <- predict(letter.ct, newdata=RPtest, type='class')
head(Prediction)

table(Prediction, RPtest$Letter)
confusionMatrix(RPtest$Letter, predictions$class)


#Something is not quite right here.
library(gbm)
Letter.gbm <- gbm(Letter ~ ., data=RPtrain, n.trees=200, interaction.depth=6, shrinkage=0.01)
Prediction = predict(Letter.gbm, newdata=RPtest, n.trees=200, type="response")
Prediction[1,,]
passing <- 0.013
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

table(PredictionClass, RPtest$Letter)
confusionMatrix(PredictionClass, RPtest$Letter)
mean(head(Prediction[,11,]))

