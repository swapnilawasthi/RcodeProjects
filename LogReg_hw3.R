#read source file
churndb <- read.csv(file.choose())
#creating test and training set as per instructions
trng.churn <- head(churndb, 1094)
test.churn <- churndb[1095:nrow(churndb),]
#perform logistic regression 
#logistic curve is not linear so, the point of the logit transform is to make it linear.
#logistic regression is linear regression on the logit transform of y, where y is the 
#proportion (or probability) of success at each value of x
lr.churn <- glm(Churn ~ ., family=binomial(link="logit"), data=trng.churn)
summary(lr.churn)
#iterations of glm by removing the non significant columns one by one, starting with least significant
lr.churn1 <- glm(Churn ~ . -Account.Length, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn1)
lr.churn2 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn2)
lr.churn3 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn3)
lr.churn4 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn4)
lr.churn5 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn5)
lr.churn6 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls - Night.Minutes, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn6)
lr.churn7 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls - Night.Minutes -Night.Calls, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn7)
lr.churn8 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls - Night.Minutes -Night.Calls-Night.Charge, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn8)
lr.churn9 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls - Night.Minutes -Night.Calls-Night.Charge-International.Minutes, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn9)
lr.churn10 <- glm(Churn ~ . -Account.Length -Voice.Mail.Plan.YES -Day.Minutes -Evening.Minutes-Evening.Calls - Night.Minutes -Night.Calls-Night.Charge-International.Minutes-International.Charge, family=binomial(link="logit"), data=trng.churn)
summary(lr.churn10)
#lets predict
pred1 <- predict(lr.churn1 , newdata=test.churn, type="response")
pred2 <- predict(lr.churn2 , newdata=test.churn, type="response")
pred3 <- predict(lr.churn3 , newdata=test.churn, type="response")
pred4 <- predict(lr.churn4 , newdata=test.churn, type="response")
pred5 <- predict(lr.churn5 , newdata=test.churn, type="response")
pred6 <- predict(lr.churn6 , newdata=test.churn, type="response")
pred7 <- predict(lr.churn7 , newdata=test.churn, type="response")
pred8 <- predict(lr.churn8 , newdata=test.churn, type="response")
pred9 <- predict(lr.churn9 , newdata=test.churn, type="response")
pred10 <- predict(lr.churn10,newdata=test.churn, type="response")
#head(pred_10)
pred_v <- c(pred1,pred2,pred3,pred4,pred5,pred6,pred7,pred8,pred9,pred10)
#converting predictions > 50% to 1 and remaining to 0
pred_1 <- ifelse(pred1 > 0.5,1,0)
pred_2 <- ifelse(pred2 > 0.5,1,0)
pred_3 <- ifelse(pred3 > 0.5,1,0)
pred_4 <- ifelse(pred4 > 0.5,1,0)
pred_5 <- ifelse(pred5 > 0.5,1,0)
pred_6 <- ifelse(pred6 > 0.5,1,0)
pred_7 <- ifelse(pred7 > 0.5,1,0)
pred_8 <- ifelse(pred8 > 0.5,1,0)
pred_9 <- ifelse(pred9 > 0.5,1,0)
pred_10 <- ifelse(pred10 > 0.5,1,0)
#table - confusion matrix
table(test.churn$Churn,pred_6)
for (i in 1:10)
{
  
  misClasificError <- mean(test.churn$Churn != pred_10)
  print(paste('Accuracy',1-misClasificError))
}
  
misClasificError <- mean(test.churn$Churn != pred_2)
print(paste('Accuracy',1-misClasificError))



