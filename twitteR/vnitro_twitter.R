#install the necessary packages
# install.packages("ROAuth")
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

library("ROAuth")
library("twitteR")
library("wordcloud")
library("tm")

#necessary step for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
cred <- OAuthFactory$new(consumerKey='SRSDocYfl7DkDmpMB93ENrDEH',
                         consumerSecret='uQdviDASZ0ohyMTdmmou0Xn0pzzcnq9daDhxP8pNzBJhebsWFP',
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

#necessary step for Windows
cred$handshake(cainfo="cacert.pem")
#save for later use for Windows
save(cred, file="twitter_authentication.Rdata")
#once saved, next time all you have to do is
#load("twitter_authentication.Rdata")
#registerTwitterOAuth(cred)

#you should get TRUE
registerTwitterOAuth(cred)
#[1] TRUE

# vnitro<- searchTwitter("@vnitro", n=100, cainfo="cacert.pem")
vnitro <- userTimeline("vnitro", n=1000, cainfo="cacert.pem")
length(vnitro)
vnitro_text <- sapply(vnitro, function(x) x$getText())
