data <- read.csv(file='Fifa17_dataset.csv')
head(fifa.d)
colnames(fifa.d)
preTe_10<-ifelse(preTe>.5,1,0)
fifadata$Name <- fifa.d$Name
colnames(fifadata)
head(fifadata)
summary(fifadata)
fifa.lr <- lm(Rating ~ . , data = trngdata)
fifadata <- subset(fifadata, select = c(30,31,32,33,))
Acceleration Speed Freekick_Accuracy Shot_Power
fifa.lr1 <- lm(Rating ~ . -Acceleration -Speed -Freekick_Accuracy -Shot_Power, data = fifadata)
nrow(fifadata)
trngdata = head(fifadata, round(0.7*nrow(fifadata)))
testdata <- tail(fifadata, round(0.3*nrow(fifadata)))
y <- read.csv(file.choose())
fifa.lgr <- glm(Rating ~ . -Acceleration -Speed -Freekick_Accuracy -Shot_Power -Interceptions -Aggression -Finishing - Penalties, family=binomial(link="logit"), data= trngdata)

fifadata$Rating<-ifelse(fifadata$Rating > 70,1,0)
fifadata <- subset(fifa.d, select = c(10,20,21,25,28,32,33,34,35,36,37,38,40,41,42,43,44,45,46,47,48))
part <-sample(1:nrow(fifadata), .66*nrow(fifadata))
trngdata <- fifadata[part,]
testdata <- fifadata[-part,]
pred <- predict(fifa.lgr, newdata=testdata, type="response")
pred_10<-ifelse(pred>.5,1,0)
table(testdata$Rating ,pred_10)
misClasificError <- mean(pred_10 != testdata$Rating)
print(paste('Accuracy',1-misClasificError))
                