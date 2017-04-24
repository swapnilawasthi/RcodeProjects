library('ROAuth')
library('twitteR')

apiKey <- 'pExoHff55qQwRD67PWS04OE81'
apiSecret <- '5Z1HcP6qXUelQjlnophEaEor9GWW5hR9UQ6hCFsmRBdpyyy6np'
actoken <- '71319672-GRNBeAYnlVbib3tueIz3KIOsH9H6bV50h3iO8APgE'
acsecret <- 'xhN46E3pxJEfpvRIdeCTljVtGXZx1Wyumn09Rn2oLg4KF'

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem") #Getting a curl certification

setup_twitter_oauth(apiKey, apiSecret, actoken, acsecret)

#Creating the authorization object by calling function OAuthFactory
twitCred <- OAuthFactory$new(consumerKey=apiKey, consumerSecret=apiSecret, requestURL='https://api.twitter.com/oauth/request_token', accessURL='https://api.twitter.com/oauth/access_token', authURL='https://api.twitter.com/oauth/authorize')

# Asking for access
twitCred$handshake(cainfo="cacert.pem")

# Save the credential for future use by downloading a Cred file at your default R folder
save(list="twitCred", file="twitteR_credentials")

#Load your credential
load("twitteR_credentials")
registerTwitterOAuth(twitCred)

#Test #extract keyword 150 tweets
#or read timeline 
#timeline.User1 <- userTimeline('@username',n=100, maxID=NULL, sinceID=NULL)
modi.tweets <- searchTwitter('NarendraModi', n=150)

#convert list to dataframe
df <- do.call("rbind", lapply(modi.tweets, as.data.frame))

#remove emoticons etc which are in latin1 encoding, convert to ASCII and replace by blank
df$text <- sapply(df$text, function(row) iconv(row, 'latin1', 'ASCII', sub=''))

#remove URLs
df$text <- gsub('(f|ht)tp(s?)://(.*)[.][a-z]+','',df$text)
#saving in a new variable
sample <- df$text

#creating positive and negative words base - consider ; as comment in txt file
pos.words <- scan('F:/Study/SDM/positive-words.txt', what = 'character', comment.char = ';')
neg.words <- scan('F:/Study/SDM/negative-words.txt', what = 'character', comment.char = ';')

#add words to the list 
pos.words <- c(pos.words,'Congrats', 'thanks','thx')
neg.words <- c(neg.words,'wtf', 'fight','shout')
#Extract the properties you want to collect (e.g., username, time, text, location, link, etc.). Here, I chose to save time, user ID, screen name, and tweet texts.
#sample.modi <- df[c("text", "created", "id", "screenName")]
  
#Save as .csv file
#write.csv(sample.modi, "file.User1.csv")
  