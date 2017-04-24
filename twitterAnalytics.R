install.packages('ROAuth')
install.packages('tau')
install.packages('twitteR')
install.packages('RCurl')
install.packages('dplyr')
#install.packages(c("twitteR", "ROAuth", "RCurl"))

library('ROAuth')
library('tau')
library('twitteR')
library('RCurl')
library('dplyr')

apiKey <- 'pExoHff55qQwRD67PWS04OE81'
apiSecret <- '5Z1HcP6qXUelQjlnophEaEor9GWW5hR9UQ6hCFsmRBdpyyy6np'
actoken <- '71319672-GRNBeAYnlVbib3tueIz3KIOsH9H6bV50h3iO8APgE'
acsecret <- 'xhN46E3pxJEfpvRIdeCTljVtGXZx1Wyumn09Rn2oLg4KF'
setup_twitter_oauth(apiKey, apiSecret, actoken, acsecret)

