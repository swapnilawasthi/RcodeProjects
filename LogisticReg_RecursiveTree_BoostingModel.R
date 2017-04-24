ChurnDF <- read.csv(file.choose())

library(ggplot2)
ggplot(ChurnDF, aes(x=Day.Minutes, y=Customer.Service.Calls,col= ChurnDF ))+geom_point()+coord_fixed(30)
selec <-sample(1:2094, .6*2094)
Training<-ChurnDF[selec,]
Testing<-ChurnDF[-selec,]
Churn.lr<-glm(Churn~Customer.Service.Calls, family=binomial(link="logit"), data=Training)
summary(Churn.lr)
preTe<-predict(Churn.lr, newdata=Testing, type="response")
preTe_10<-ifelse(preTe>.5,1,0)
table(Testing$Churn,preTe_10)
Churn1.lr<-glm(Churn~., family=binomial(link="logit"), data=Training)
summary(Churn1.lr)
Churn1.lr<-glm(Churn~International.Plan.YES+Voice.Mail.Plan.YES+Day.Minutes+Evening.Minutes+Night.Minutes+Customer.Service.Calls+International.Calls, family=binomial(link="logit"), data=Training)
summary(Churn1.lr)
preTe1<-predict(Churn1.lr, newdata=Testing, type="response")
preTe1_10<-ifelse(preTe1>.5,1,0)
table(Testing$Churn,preTe1_10)

Churn2.LogReg<-glm(Churn~
                     International.Plan.YES
                   +Voice.Mail.Plan.YES
                   +Day.Minutes
                   +Night.Charge
                   + Customer.Service.Calls
                   +International.Calls,
                   family=binomial,data=Churn)
summary(Churn2.LogReg)
Churn= read.csv(file.choose())
head(Churn)
train <- Churn[1:1094,]
test <- Churn[1095:2094,]
Churn.LogReg <- glm(Churn~., data=train)
summary(Churn.LogReg)
fitted.results <- predict(Churn2.LogReg,subset(test,select=c(2,3,5,13,15,17)),type='response')
head(fitted.results)
fitted.results <- ifelse(fitted.results > 0.5,1,0)
head(fitted.results)
head(test)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy',1-misClasificError))

# Recursive Tree
selec<-sample(1:2094, .6*2094)
Training<-ChurnDF[selec,]
Testing<-ChurnDF[-selec,]
library(ggplot2)
ggplot(Training, aes(x=Day.Minutes, y=Customer.Service.Calls, col=Churn))+geom_point()

library(rpart)
Churn.ct<-rpart(as.factor(Churn)~., data=Training[,1:18], cp=0)
plot(Churn.ct)
text(Churn.ct, use.n = T, digits = 3, cex = 0.6)
#Generalized Boosted Models
summary(Churn.ct)
Churn.ct<-rpart(as.factor(Churn)~., data=Training, cp=0.03)
preTe<-predict(Churn.ct, newdata=Testing, type='class')
table(preTe,Testing[,18])
library(gbm)
Churn.gbm<-gbm(Churn~., data=Training, n.trees=5000, interaction.depth=6,shrinkage=0.01)
preTeg<-predict(Churn.gbm, newdata=Testing, n.trees=5000, type="response")
preTegC<-ifelse(preTeg>.5,1,0)
table(Testing[,18], preTegC)
  
