install.packages('klaR')
library("caret")
library ("e1071")
library ("klaR")

setwd('F:/Study/SDM/HW4')

sp500 <- read.csv('SP500data25yrs.csv')
sp500$Date <- as.Date(sp500$Date, '%Y-%m-%d')
sp500.d <- sp500[,c('Date','Adj.Close')]
colnames(sp500.d)[2] <- 'snp_adj_close'

bond10yrdata <- read.csv('10_Yr_Bond_rates.csv')
bond10yrdata$Date <- as.Date(bond10yrdata$Date, '%Y-%m-%d')
bond10yr.d <- bond10yrdata[,c('Date', 'Adj.Close')]
colnames(bond10yr.d)[2] <- 'bond10yr_adj_close'


crudeoil <- read.csv('crudeoil.csv')
crudeoil$DATE <- as.Date(crudeoil$DATE, '%Y-%m-%d')
crudeoil$VALUE <- as.numeric(crudeoil$VALUE)
colnames(crudeoil)[2] <- 'crude_oil_price_usd'

# check missing values
colSums(is.na(bond10yr.d))
colSums(is.na(sp500.d))
colSums(is.na(crudeoil))
bond10yr <- bond10yr.d
snp500 <- sp500.d

install.packages('sqldf')
library('sqldf')

merged_df <- sqldf('SELECT *
                   FROM snp500 
                   INNER JOIN bond10yr ON (snp500.Date = bond10yr.Date)
                   INNER JOIN crudeoil ON (snp500.Date = crudeoil.DATE)')

merged_df <- merged_df[, c('Date', 'snp_adj_close', 'bond10yr_adj_close', 'crude_oil_price_usd')]

# compute price change for snp500
snp_pc1 = c(); snp_pc2= c(); snp_pc3 = c(); snp_pc4 = c(); snp_pc5 = c(); priceDir = c();
for (i in 7:nrow(merged_df)){
  snp_pc1[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 2]) 
  snp_pc2[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 3]) 
  snp_pc3[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 4]) 
  snp_pc4[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 5]) 
  snp_pc5[i] <- (merged_df$snp_adj_close[i - 1] - merged_df$snp_adj_close[i - 6]) 
  priceDir[i] <- ifelse((merged_df$snp_adj_close[i] - merged_df$snp_adj_close[i - 1]) > 0, 'High', 'Low')
}

# compute price change for bond10yr
bnd_pc1 = c(); bnd_pc2= c(); bnd_pc3 = c(); bnd_pc4 = c(); bnd_pc5 = c();
for (i in 7:nrow(merged_df)){
  bnd_pc1[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 2]) 
  bnd_pc2[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 3]) 
  bnd_pc3[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 4]) 
  bnd_pc4[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 5]) 
  bnd_pc5[i] <- (merged_df$bond10yr_adj_close[i - 1] - merged_df$bond10yr_adj_close[i - 6]) 
}

# compute price change for crude oil
oil_pc1 = c(); oil_pc2= c(); oil_pc3 = c(); oil_pc4 = c(); oil_pc5 = c();
for (i in 7:nrow(merged_df)){
  oil_pc1[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 2]) 
  oil_pc2[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 3]) 
  oil_pc3[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 4]) 
  oil_pc4[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 5]) 
  oil_pc5[i] <- (merged_df$crude_oil_price_usd[i - 1] - merged_df$crude_oil_price_usd[i - 6]) 
}

# create categorical values for snp
snp.sd = sd(merged_df$snp_adj_close)
snp_cat1 = c(); snp_cat2= c(); snp_cat3 = c(); snp_cat4 = c(); snp_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  snp_cat1[i] = ifelse(snp_pc1[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc1[i] >= -1 * snp.sd && snp_pc1[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc1[i] >= -0.3 * snp.sd && snp_pc1[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc1[i] >= 0.3 * snp.sd && snp_pc1[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc1[i] >= snp.sd), 'Great', 'None')))))))))
  snp_cat2[i] = ifelse(snp_pc2[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc2[i] >= -1 * snp.sd && snp_pc2[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc2[i] >= -0.3 * snp.sd && snp_pc2[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc2[i] >= 0.3 * snp.sd && snp_pc2[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc2[i] >= snp.sd), 'Great', 'None')))))))))
  
  snp_cat3[i] = ifelse(snp_pc3[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc3[i] >= -1 * snp.sd && snp_pc3[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc3[i] >= -0.3 * snp.sd && snp_pc3[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc3[i] >= 0.3 * snp.sd && snp_pc3[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc3[i] >= snp.sd), 'Great', 'None')))))))))
  
  snp_cat4[i] = ifelse(snp_pc4[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc4[i] >= -1 * snp.sd && snp_pc4[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc4[i] >= -0.3 * snp.sd && snp_pc4[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc4[i] >= 0.3 * snp.sd && snp_pc4[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc4[i] >= snp.sd), 'Great', 'None')))))))))
  snp_cat5[i] = ifelse(snp_pc5[i] < -1 * snp.sd, 'awful', 
                       (ifelse((snp_pc5[i] >= -1 * snp.sd && snp_pc5[i] < -0.3 * snp.sd), 'Bad',
                               (ifelse((snp_pc5[i] >= -0.3 * snp.sd && snp_pc5[i] < 0.3 * snp.sd), 'Unchanged',
                                       (ifelse((snp_pc5[i] >= 0.3 * snp.sd && snp_pc5[i] < snp.sd), 'Good',
                                               (ifelse((snp_pc5[i] >= snp.sd), 'Great', 'None')))))))))
  
  
}

# create categorical values for 10yrbond
bnd.sd = sd(merged_df$bond10yr_adj_close)
bnd_cat1 = c(); bnd_cat2= c(); bnd_cat3 = c(); bnd_cat4 = c(); bnd_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  bnd_cat1[i] = ifelse(bnd_pc1[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc1[i] >= -1 * bnd.sd && bnd_pc1[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc1[i] >= -0.3 * bnd.sd && bnd_pc1[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc1[i] >= 0.3 * bnd.sd && bnd_pc1[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc1[i] >= bnd.sd), 'Great', 'None')))))))))
  bnd_cat2[i] = ifelse(bnd_pc2[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc2[i] >= -1 * bnd.sd && bnd_pc2[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc2[i] >= -0.3 * bnd.sd && bnd_pc2[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc2[i] >= 0.3 * bnd.sd && bnd_pc2[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc2[i] >= bnd.sd), 'Great', 'None')))))))))
  
  bnd_cat3[i] = ifelse(bnd_pc3[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc3[i] >= -1 * bnd.sd && bnd_pc3[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc3[i] >= -0.3 * bnd.sd && bnd_pc3[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc3[i] >= 0.3 * bnd.sd && bnd_pc3[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc3[i] >= bnd.sd), 'Great', 'None')))))))))
  
  bnd_cat4[i] = ifelse(bnd_pc4[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc4[i] >= -1 * bnd.sd && bnd_pc4[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc4[i] >= -0.3 * bnd.sd && bnd_pc4[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc4[i] >= 0.3 * bnd.sd && bnd_pc4[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc4[i] >= bnd.sd), 'Great', 'None')))))))))
  bnd_cat5[i] = ifelse(bnd_pc5[i] < -1 * bnd.sd, 'awful', 
                       (ifelse((bnd_pc5[i] >= -1 * bnd.sd && bnd_pc5[i] < -0.3 * bnd.sd), 'Bad',
                               (ifelse((bnd_pc5[i] >= -0.3 * bnd.sd && bnd_pc5[i] < 0.3 * bnd.sd), 'Unchanged',
                                       (ifelse((bnd_pc5[i] >= 0.3 * bnd.sd && bnd_pc5[i] < bnd.sd), 'Good',
                                               (ifelse((bnd_pc5[i] >= bnd.sd), 'Great', 'None')))))))))
  
}

# create categorical values for 10yrbond
oil.sd = sd(merged_df$crude_oil_price_usd)
oil_cat1 = c(); oil_cat2= c(); oil_cat3 = c(); oil_cat4 = c(); oil_cat5 = c();
for (i in 7:nrow(merged_df)){
  
  oil_cat1[i] = ifelse(oil_pc1[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc1[i] >= -1 * oil.sd && oil_pc1[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc1[i] >= -0.3 * oil.sd && oil_pc1[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc1[i] >= 0.3 * oil.sd && oil_pc1[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc1[i] >= oil.sd), 'Great', 'None')))))))))
  oil_cat2[i] = ifelse(oil_pc2[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc2[i] >= -1 * oil.sd && oil_pc2[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc2[i] >= -0.3 * oil.sd && oil_pc2[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc2[i] >= 0.3 * oil.sd && oil_pc2[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc2[i] >= oil.sd), 'Great', 'None')))))))))
  
  oil_cat3[i] = ifelse(oil_pc3[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc3[i] >= -1 * oil.sd && oil_pc3[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc3[i] >= -0.3 * oil.sd && oil_pc3[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc3[i] >= 0.3 * oil.sd && oil_pc3[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc3[i] >= oil.sd), 'Great', 'None')))))))))
  
  oil_cat4[i] = ifelse(oil_pc4[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc4[i] >= -1 * oil.sd && oil_pc4[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc4[i] >= -0.3 * oil.sd && oil_pc4[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc4[i] >= 0.3 * oil.sd && oil_pc4[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc4[i] >= oil.sd), 'Great', 'None')))))))))
  oil_cat5[i] = ifelse(oil_pc5[i] < -1 * oil.sd, 'awful', 
                       (ifelse((oil_pc5[i] >= -1 * oil.sd && oil_pc5[i] < -0.3 * oil.sd), 'Bad',
                               (ifelse((oil_pc5[i] >= -0.3 * oil.sd && oil_pc5[i] < 0.3 * oil.sd), 'Unchanged',
                                       (ifelse((oil_pc5[i] >= 0.3 * oil.sd && oil_pc5[i] < oil.sd), 'Good',
                                               (ifelse((oil_pc5[i] >= oil.sd), 'Great', 'None')))))))))
  
}

# combine all the columns for the final time and remove the top 7 rows that have NA value for categories
final_merged_df <- data.frame(priceDir, snp_cat1, snp_cat2, snp_cat3, snp_cat4, snp_cat5, bnd_cat1, bnd_cat2, bnd_cat3, bnd_cat4, bnd_cat5, oil_cat1, oil_cat2, oil_cat3, oil_cat4, oil_cat5 )
final_merged_df <- tail(final_merged_df, -6)



#partition
rnum <- (runif(1, .60, .70))
rnum
part <-sample(1:nrow(final_merged_df), rnum * nrow(final_merged_df))
trng.d <- final_merged_df[part,]
test.d <- final_merged_df[-part,]


# Modelling using NaiveBayes
model.NB <- NaiveBayes(priceDir ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5 ,data=trng.d)
predictions <- predict(model.NB, test.d)
confusionMatrix(test.d$priceDir, predictions$class)

# Modelling using Recursive Partition Tree
#install.packages('rpart')
library('rpart')
model.rpt <- rpart(priceDir ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5, data=trng.d, cp=0)
plot(model.rpt)
text(model.rpt, use.n= T, digits=3, cex=0.6)
prediction.rpt <- predict(model.rpt, newdata = test.d, type="class")
printcp(model.rpt)
table(prediction.rpt, test.d$priceDir)

# Modelling using Gradient Boosting
#install.packages('gbm')
library('gbm')
model.gbm <- gbm((unclass(priceDir)-1) ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5, data=trng.d, n.trees=5000, interaction.depth =6, shrinkage=0.01)
prediction.gbm <- predict(model.gbm, newdata = test.d, n.trees=5000, type="response")
head(prediction.gbm[])
tail(prediction.gbm[])
summary(gbm1,n.trees=1)

#cross validation
control <- trainControl(method = "cv", number = 10 , savePredictions = TRUE)
price.rpt.cv <- train(priceDir ~ snp_cat3+ snp_cat4 + snp_cat5 + bnd_cat4 + bnd_cat5 + oil_cat1 + oil_cat2 + oil_cat3 + oil_cat4 + oil_cat5, data = trng.d , method = "rpart", trControl = control)
predict.rpt.cv <- predict(price.rpt.cv, newdata = test.d)
confusionMatrix(predict.rpt.cv, test.d$priceDir)








