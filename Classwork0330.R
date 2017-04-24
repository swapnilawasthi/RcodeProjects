HoursPass = read.csv(file.choose())
#generalized linear model,
HP.LR = glm(formula=Pass~Hours, family=binomial(link="logit"), data=HoursPass)
intercept = HP.LR$coefficients[1]
slope = HP.LR$coefficients[2]
iStudy = 3.6
exp(intercept + slope*iStudy)/(1+exp(intercept+slope*iStudy))

Subs = read.csv(file.choose())
#generalized linear model,
sub.LR = glm(formula= Subscribe. ~ ., family=binomial(link="logit"), data=Subs)
intercept = sub.LR$coefficients[1]
slope = sub.LR$coefficients[2]
iStudy = 80 #diff value of iStudy
exp(intercept + slope*iStudy)/(1+exp(intercept+slope*iStudy))
summary(sub.LR)

filmdata <- read.table(file.choose(), header = TRUE)
film.LR = glm(formula= Oscar ~ BoxOffice, family=binomial, data=filmdata)
intercept = film.LR$coefficients[1]
slope = film.LR$coefficients[2]
iboxoffice = 80 #diff value of iStudy
exp(intercept + slope*iboxoffice)/(1+exp(intercept+slope*iboxoffice))
summary(film.LR)

#for all the cols
filmdata <- read.table(file.choose(), header = TRUE)
film.LR = glm(formula= Oscar ~ ., family=binomial, data=filmdata)
intercept = film.LR$coefficients[1]
slope = film.LR$coefficients[2]
iboxoffice = 50 #diff value of iStudy
box50 <- exp(intercept + slope*iboxoffice)/(1+exp(intercept+slope*iboxoffice))
iboxoffice = 51 #diff value of iStudy
box51 <- exp(intercept + slope*iboxoffice)/(1+exp(intercept+slope*iboxoffice))
box51 - box50
summary(film.LR)

filmdata <- read.table(file.choose(), header = TRUE)
film.LR = glm(formula= Oscar ~ (Critics+BoxOffice), family=binomial, data=filmdata)
intercept = film.LR$coefficients[1]
slope = film.LR$coefficients[2]
iboxoffice = 50 #diff value of iStudy
box50 <- exp(intercept + slope*iboxoffice)/(1+exp(intercept+slope*iboxoffice))
predict(film.LR,data.frame(Critics=90,BoxOffice=100)) #running for critics and boxoffice value


filmdata <- read.table(file.choose(), header = TRUE)
film.LR = glm(formula= Oscar ~ (Critics+BoxOffice+Country), family=binomial, data=filmdata)
intercept = film.LR$coefficients[1]
BOslope = film.LR$coefficients[3]
criticslope = film.LR$coefficients[2]
Indiaslope = film.LR$coefficients[5]
iboxoffice = 100 #diff value of iStudy
icritic = 90
icountryindia = 1
exp(intercept+ BOslope*iboxoffice+criticslope*icritic+Indiaslope*icountryindia)/(1+exp(intercept+BOslope*iboxoffice+criticslope*icritic+Indiaslope*icountryindia))
box50 <- exp(intercept + slope*iboxoffice)/(1+exp(intercept+slope*iboxoffice))
predict(film.LR,data.frame(Critics=90,BoxOffice=100,Country='India')) #running for critics and boxoffice value

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata

mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv") 
xtabs(~ admit + rank, data = mydata) 
mydata$rank <- factor(mydata$rank) 
mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial") 
summary(mylogit) 
confint(mylogit) 
newdata1 <- with(mydata,data.frame(gre = mean(gre), gpa = mean(gpa), rank = factor(1:4))) 
newdata1 
newdata1$rankP <- predict(mylogit, newdata = newdata1, type = "response") 
newdata1 
newdata2 <- with(mydata, data.frame(gre = rep(seq(from = 200, to = 800, length.out = 100), 4), gpa = mean(gpa), rank = factor(rep(1:4, each = 100)) )) 
head(newdata2) 
newdata3 <- cbind(newdata2, predict(mylogit, newdata = newdata2, type="link", se=TRUE)) 
newdata3 <- within(newdata3, { PredictedProb <- plogis(fit) 
LL <- plogis(fit - (1.96 * se.fit)) 
UL <- plogis(fit + (1.96 * se.fit)) }) 
head(newdata3) 
ggplot(newdata3, aes(x = gre, y = PredictedProb)) + geom_ribbon(aes(ymin = LL, ymax = UL, fill = rank), alpha = .2) + geom_line(aes(colour = rank), size=1)



