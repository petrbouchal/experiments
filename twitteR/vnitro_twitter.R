#install the necessary packages
# install.packages("ROAuth")
# install.packages("twitteR")
# install.packages("wordcloud")
# install.packages("tm")

library("pbtools")
library("ROAuth")
library("twitteR")
library("tidyr")
library('car')
# library("wordcloud")
# library("tm")

#necessary step for Windows
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

#to get your consumerKey and consumerSecret see the twitteR documentation for instructions
cred <- setup_twitter_oauth(consumer_key='SRSDocYfl7DkDmpMB93ENrDEH',
                         consumer_secret='uQdviDASZ0ohyMTdmmou0Xn0pzzcnq9daDhxP8pNzBJhebsWFP',
                         access_token = '223195622-UslBPSY63p8IpnnDrJdvNKyO7aiWm6uOpMbg3vWz',
                         access_secret = 'aq0Sz72EWMt2Qqtyp7cE99qGs6tnH6OW6Bz8OovsdwubE'
                         )

#necessary step for Windows
# cred$handshake(cainfo="cacert.pem")
#save for later use for Windows
# save(cred, file="twitter_authentication.Rdata")
#once saved, next time all you have to do is
#load("twitter_authentication.Rdata")
#registerTwitterOAuth(cred)

# vnitro<- searchTwitter("@vnitro", n=100)
vnitro <- userTimeline("vnitro", n=1000)
length(vnitro)
vnitro_text <- sapply(vnitro, function(x) x$getText())
tvnitro <- data.frame(text=vnitro_text)
tvnitro$polic <- !is.na(str_match(tvnitro$text, '[Pp]olic'))
tvnitro$hasi <- !is.na(str_match(tvnitro$text, '[Hh]asi'))
tvnitro$teror <- !is.na(str_match(tvnitro$text, '[tT]eror'))
tvnitro$ss <- !is.na(str_match(tvnitro$text, '[Ss]tátní služ'))
tvnitro$sport <- !is.na(str_match(tvnitro$text, '[Ss]port'))
tvnitro$bezpe <- !is.na(str_match(tvnitro$text, '[Bb]ezpe'))
tvnitro$munice <- !is.na(str_match(tvnitro$text, '[Mm]uni'))

vl <- melt(tvnitro, id.vars = 'text')
vl2 <- vl %>% group_by(variable) %>% select(-text) %>% summarise_each('mean')
vl2$variable <- recode(vl2$variable, "'polic'='Policie';'bezpe'='Bezpečnost';
                       'ss'='Státní služba';'hasi'='Hasiči';'sport'='Sport';
                       'teror'='Terorismus';'munice'='Munice'")
vl2$variable <- reorder(vl2$variable, vl2$value)

loadcustomthemes(themebasecolours,'Helvetica')
ggplot(vl2, aes(variable, value)) +
  geom_bar(stat = 'identity', fill='light blue') +
  scale_y_continuous(labels=percent) +
  coord_flip() +
  theme(axis.text.y=element_text(size=15, face='bold'),
        panel.grid.major.y=element_blank(),
        axis.text.x=element_text(size = 13),
        panel.grid.major.x=element_line())
