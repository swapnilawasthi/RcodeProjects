data<- read.csv(file.choose())
data<- read.csv('Consumer_Complaints_Small.csv', header = TRUE, stringsAsFactors = FALSE)

head(data)
data$Date.received <- as.Date(data$Date.received, "%m/%d/%Y")
head(data$Date.received)

class(data$Date.received)
data$Date.received <- strftime(data$Date.received, format="%Y-%m")
setwd('F:/Study/SDM')
data.cc <- read.csv('Consumer_Complaints_Small.csv', header = TRUE)

data.cc.sub <- subset(data.cc, data.cc$Date.received >= "4/1/2012" & data.cc$Date.received <= "6/30/2016")

data.cc <- subset(data.cc)
data.cc.sub <- subset(data.cc, data.cc$Date.received > '2012-03-31' & data.cc$Date.received < '2016-7-1')
data.sloan <- subset(data.cc.sub, data.cc.sub$Product == 'Student loan')
conn<-dbConnect(MySQL(), user='awasthi',password='awasthi',db_name='db',host='db.cxaizxz7riof.us-west-2.rds.amazonaws.com')

