data <- read.csv(file.choose());
head(data);
adv.lm <- lm(Sales~TV, data=data);
summary(adv.lm);
data.adv <- data;
plot(Sales~TV, data = data.adv, col='blue');
pred <- predict(adv.lm);
points(data.adv$TV,pred, col='orange');
library(caret)
mtcars.lm <- train(mpg~hp, data=mtcars, method = 'lm');
model.mtcars.lm <- train(mpg~wt, data=mtcars, method = 'lm');

model.mtcars_lm <- train(mpg ~ wt, data=mtcars, method="lm") 
coef.icept <- coef(model.mtcars_lm$finalModel)[1] 
coef.slope <- coef(model.mtcars_lm$finalModel)[2]
ggplot(data=mtcars, aes(x=wt, y=mpg)) + geom_point() + geom_abline(slope=coef.slope, intercept=coef.icept, color="red")

subset75 <- createDataPartition(y=mtcars$mpg, p=.75, list=FALSE) #subset75
training <- mtcars[subset75,] #training 
testing <- mtcars[-subset75,] #Testing
mtcarReg <- train(mpg~., data=training, method="lm") 
summary(mtcarReg)

