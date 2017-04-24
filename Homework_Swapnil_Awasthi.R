###verifying the working directory
getwd()
###read the input file
data.cc<- read.csv('Consumer_Complaints_Small.csv', header = TRUE, stringsAsFactors = FALSE)
###first 6 lines of the data
head(data.cc)
###converting the format as 'Date'
data.cc$Date.received <- as.Date(data.cc$Date.received, "%m/%d/%Y")
###first 6 lines of the data
head(data.cc$Date.received)
###verifyung the class of the changed date
class(data.cc$Date.received)
###subsetting the selected range of dates
data.sub <- subset(data.cc, data.cc$Date.received > '2012-03-31' & data.cc$Date.received < '2016-7-1')
###subsetting the data with student loan as product
data.loan <- subset(data.sub, data.sub$Product == 'Student loan')
###calculating the month and year from the student loan data
loan_period <- as.yearmon(data.loan$Date.received, format("%Y %m"))
###calculating the count of rows for each month using aggregate function
data.month <- aggregate(data.loan$Date.received ~ loan_period, FUN=length)
###number of rows we have
nrow(data.month)
###creating a dataset for the number of rows in the data above
data.month.df <- data.frame(c(1:nrow(data.month)),data.month)
###naming the columns
names(data.month.df)[1] = "Index"
names(data.month.df)[2] = "MonthYear"
names(data.month.df)[3] = "Frequency"
###training set
training <- head(data.month.df, 45)
###linear regression model
data.lm <- lm(Frequency~Index, data=training)
###plot the graph
plot(training$Index, training$Frequency)
###adding a line to fit the model
abline(data.lm, col="red") # this is not printing a line for me!!!!
###summary of the regression model
summary(data.lm)
###equation of the line: Frequency = 3.7014*Index + 228.3111
###actual data for the remaining 6/51 rows
tail(data.month.df,6)
data.month.df


