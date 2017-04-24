data <- read.csv(file.choose());
nrow(data);
getwd();
setwd("F:/Study/SDM/HW2");
data.new <- tail(data, -11);
data.new<- read.csv(file="SourceFile.csv",header = TRUE, nrows=(nrow(data)-10));
data.cc<- read.csv('SourceFile.csv', header = TRUE, stringsAsFactors = FALSE);
#removing the first 10 lines
data.new <- tail(data, -11);
head(data);
head(data.new);
###
#reading the 11th row for the column names
headers = read.csv(file="SourceFile.csv", skip = 10, header = FALSE, nrows = 1, as.is = T);
nrow(headers);
#reading data from 13th row for the actual data
data <- read.csv(file="SourceFile.csv", skip = 12, header = FALSE);
#setting up the default column name as headers
colnames(data) <- headers;
#making sure there are no white spaces before and after the month name
data$Month <- trimws(data$Month,"both");
#extracting the year and month from the column name
data.year <- substr(data$Month,1,4);
data.month <- substr(data$Month,6,length(data$Month));
data.index <- seq(1,nrow(data));
#creating a data frame as shown the homework assignment
data.df <- data.frame(Period=data.index,Year=as.integer(data.year),Month=data.month, CO2Emissions = data$'Coal, Including Coal Coke Net Imports, CO2 Emissions')

#creating training and test set
trng.data <- subset(data.df, data.df$Year < 2016);
test.data <- subset(data.df, data.df$Year == 2016);
#perform regression
linear.reg <- lm(CO2Emissions ~ Period, data = trng.data);
#plotting the data points
plot(trng.data$Period, trng.data$CO2Emissions);
#fitting the regression line
abline(linear.reg, col='orange');
summary(linear.reg);
#root mean square error
linear.rmse  <- sqrt(mean(linear.reg$residuals)^2);
#predicting the test data
linear.predict <- predict(linear.reg,test.data);

####quadratic regression Period^2

#creating training and test set
trng.data$Periodsq <- trng.data$Period^2;
test.data$Periodsq <- test.data$Period^2;

quad.reg <- lm(CO2Emissions ~ Period + Periodsq, data = trng.data);
#plotting the data points
plot(trng.data$Period, trng.data$CO2Emissions);
#fitting the regression line
abline(quad.reg, col='red');
summary(quad.reg);
#root mean square error
quad.rmse  <- sqrt(mean(quad.reg$residuals)^2);
#predicting the test data
quad.predict <- predict(quad.reg,test.data);

### regression using month
quad.reg.m <- lm(CO2Emissions ~ Period + Periodsq + Month, data = trng.data);
#predicting the test data
quad.predict.m <- predict(quad.reg.m,test.data);
#root mean square error
quad.rmse.m  <- sqrt(mean(quad.reg.m$residuals)^2);


#creating training and test set for period 4 and period 3
trng.data$Period3 <- trng.data$Period^3;
test.data$Period3 <- test.data$Period^3;

trng.data$Period4 <- trng.data$Period^4;
test.data$Period4 <- test.data$Period^4;

poly.reg <- lm(CO2Emissions ~ Period + Periodsq + Period3 + Period4 + Month, data = trng.data);
#predicting the test data
poly.predict <- predict(poly.reg,test.data);
#root mean square error
poly.rmse  <- sqrt(mean(poly.reg$residuals)^2);


