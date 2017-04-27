####################################
# Key for user 1234 #
####################################

StudentId4 = 1234   #Change this to the last 4 digits above
set.seed(StudentId4)

# ##############
# Question 1
################
require(caret)
Deficiencies = read.csv(file.choose())
head(Deficiencies, 10)
Subset75 <- createDataPartition(y=Deficiencies$tag, p=.75, list=FALSE)
training <- Deficiencies[Subset75,]
testing <- Deficiencies[-Subset75,]
write.csv(training, "DeficienciesTraining.csv")
write.csv(testing, "DeficienciesTesting.csv")

# Convert the statdate into a date structure
training$statdate = as.Date(training$statdate)
testing$statdate = as.Date(testing$statdate)
Deficiencies$statdate = as.Date(Deficiencies$statdate)

# consider only 2014 and 2015
training <- subset(training, statdate >= as.Date("2014-01-01"))
testing <- subset(testing, statdate >= as.Date("2014-01-01"))
training <- subset(training, statdate <= as.Date("2015-12-31"))
testing <- subset(testing, statdate <= as.Date("2015-12-31"))
# consider FL only
training <- subset(training, state=="FL")
testing <- subset(testing, state=="FL")
head(training)

testing$YearMonth <- strftime(testing$statdate, format="%Y-%m")
training$YearMonth <- strftime(training$statdate, format="%Y-%m")  

library(sqldf)
TrainingDefByMonth <- sqldf('select YearMonth, count(YearMonth) as Deficiencies from training group by YearMonth order by YearMonth')
TrainingDefByMonth$Period = c(1:24)
head(TrainingDefByMonth)

# Question d:  Predict for time period 25:
Def.lm = lm(Deficiencies~Period, data=TrainingDefByMonth)
summary(Def.lm)
predict(Def.lm)
predict(Def.lm, data.frame(Period=25))
#Answer is given below
plot(TrainingDefByMonth$Deficiencies ~ TrainingDefByMonth$Period)
points(predict(Def.lm), col="blue")

# Question e:  Predict all deficits for period 25
training = read.csv("DeficienciesTraining.csv")
training$statdate = as.Date(training$statdate)
training <- subset(training, statdate >= as.Date("2014-01-01"))
training <- subset(training, statdate <= as.Date("2015-12-31"))
training$YearMonth <- strftime(training$statdate, format="%Y-%m")  
TrainingDefByMonth <- sqldf('select YearMonth, count(YearMonth) as Deficiencies from training group by YearMonth order by YearMonth')
TrainingDefByMonth$Period = c(1:24)
Def.lm = lm(Deficiencies~Period, data=TrainingDefByMonth)
summary(Def.lm)
predict(Def.lm)
predict(Def.lm, data.frame(Period=25))
#Answer is given below
plot(TrainingDefByMonth$Deficiencies ~ TrainingDefByMonth$Period)
points(predict(Def.lm), col="blue")

##############
# Question 2
##############

set.seed(StudentId)
RandomFactor = colMeans(replicate(208,runif(StudentId))+.5)
BankData = read.csv(file.choose())
BankData$Salary = round(BankData$Salary  * RandomFactor, digits=0)
BankData$EducLev <- as.factor(BankData$EducLev)
BankData$JobGrade <- as.factor(BankData$JobGrade)
BankData$YearsWithFirm <- 95 - BankData$YrHired
BankData$Age <- 95 - BankData$YrBorn
Bank.reg = lm(Salary ~ EducLev + JobGrade + Age + Gender, data=BankData)
# Question a:
summary(Bank.reg)$r.squared
# Question b:
predict(Bank.reg,data.frame(EducLev="4", JobGrade="3", Gender="Male", Age=30))
# Question c:
predict(Bank.reg,data.frame(EducLev="4", JobGrade="3", Gender="Female", Age=30))
# Question d:
BankData$TotalYears = BankData$YearsWithFirm + BankData$YrsPrior
Bank.reg2 = lm(Salary ~ JobGrade + TotalYears + Gender + PCJob, data=BankData)
summary(Bank.reg2)$r.squared
# Question e:
predict(Bank.reg2, data.frame(Gender="Male", Age=30, JobGrade="3", TotalYears=5, PCJob="No"))
# Question f:
predict(Bank.reg2, data.frame(Gender="Male", Age=30, JobGrade="3", TotalYears=5, PCJob="Yes"))
# Question g:
predict(Bank.reg2, data.frame(Gender="Female", Age=30, JobGrade="3", TotalYears=5, PCJob="No"))
# Question h:
predict(Bank.reg2, data.frame(Gender="Female", Age=30, JobGrade="3", TotalYears=5, PCJob="Yes"))

# Question i
BankData207 = BankData[-208,]
Bank.reg3 = lm(Salary ~ JobGrade + TotalYears + Gender + PCJob, data=BankData207)
summary(Bank.reg3)$r.squared
# Question j
predict(Bank.reg3, data.frame(Gender="Male", Age=30, JobGrade="3", TotalYears=5, PCJob="No"))
# Question k
predict(Bank.reg3, data.frame(Gender="Male", Age=30, JobGrade="3", TotalYears=5, PCJob="Yes"))

###############
# Question 3
###############

set.seed(StudentId)
RandomFactor = colMeans(replicate(25,runif(StudentId))+.5)
InvestmentData = read.csv(file.choose())
InvestmentData$Value = round(InvestmentData$Investment.Value  * RandomFactor, digits=0)
# Question a
plot(Value ~ Year, data=InvestmentData, col="red")
InvestmentData$LogValue = log(InvestmentData$Value)
Investment.reg = lm(LogValue ~ Year, data=InvestmentData)
Investment.reg$coefficients
InvestmentData$Prediction = exp(Investment.reg$coefficients[1]) * exp(Investment.reg$coefficients[2]*InvestmentData$Year)
points(InvestmentData$Prediction, col="blue")
# Question b
FiveYearPrediction = data.frame(c(26:30))
names(FiveYearPrediction) <- c("Year")
FiveYearPrediction$Prediction = exp(Investment.reg$coefficients[1]) * exp(Investment.reg$coefficients[2]*FiveYearPrediction$Year)
head(FiveYearPrediction,5)

################
# Question 4
################

set.seed(StudentId)
RandomFactor = colMeans(replicate(1000,2*runif(StudentId)))
Catalog = read.csv(file.choose())
Catalog$AmountSpent = round(Catalog$AmountSpent  * RandomFactor, digits=0)
RandomFactor = colMeans(replicate(1000,2*runif(StudentId)))
Catalog$Salary = round(Catalog$Salary  * RandomFactor, digits=0)
Subset75 <- createDataPartition(y=Catalog$AmountSpent, p=.75, list=FALSE)
ctraining <- Catalog[Subset75,]
ctesting <- Catalog[-Subset75,]
head(ctraining)
head(ctesting)
# Question
Catalog.reg = lm(AmountSpent ~ ., data=data.frame(ctraining))
summary(Catalog.reg)
summary(Catalog.reg)$sigma
# Next question
# I will look at the RMSE for each
trainingMSE = mean(summary(Catalog.reg)$residuals^2)

testingPrediction = predict(Catalog.reg, ctesting)
head(testingPrediction)
ctesting$Prediction = testingPrediction
testingMSE = mean((ctesting$AmountSpent - ctesting$Prediction)^2)

RMSETraining = sqrt(trainingMSE)
RMSETesting = sqrt(testingMSE)

cat("Testing: ", RMSETraining , " Testing: " , RMSETesting)



