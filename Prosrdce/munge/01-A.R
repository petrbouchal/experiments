# Example preprocessing script.
name <- c('Praha','Brno')
stn <- unique(WeatherDailyBrnoPraha$STN...)
stations <- data.frame('STN...'=stn,'STNname'=name)
weather <- merge(WeatherDailyBrnoPraha,stations)
analytics <- Analytics.www.prosrdce.cz.Audience.Overview.20100701.20130731
rm(Analytics.www.prosrdce.cz.Audience.Overview.20100701.20130731)
rm(WeatherDailyBrnoPraha)
rm(stn)
rm(name)

analytics$date <- as.Date(analytics$Day,format='%m/%d/%Y')
analytics$sdate <- as.character(analytics$date)
analytics$year <- sapply(strsplit(analytics$sdate, "-"), `[`, 1)
analytics$month <- sapply(strsplit(analytics$sdate, "-"), `[`, 2)
analytics$day <- sapply(strsplit(analytics$sdate, "-"), `[`, 3)

analytics$year[analytics$year=='0010'] <- '2010'
analytics$year[analytics$year=='0011'] <- '2011'
analytics$year[analytics$year=='0012'] <- '2012'

analytics$ndate <- paste(analytics$year,analytics$month,analytics$day,sep='-')
analytics$fdate <- as.Date(analytics$ndate,format='%Y-%m-%d')

weather$fdate <- as.Date(as.character(weather$YEARMODA),format='%Y%m%d')
weather$MAX <- gsub('*','',as.character(weather$MAX))
weather$MAX <- as.numeric(as.character(weather$MAX))
weather$MIN <- gsub('*','',as.character(weather$MIN))
weather$MIN <- as.numeric(as.character(weather$MIN))

analytics$Visits <- as.numeric(as.character(analytics$Visits))

uu <- merge(weather,analytics)

uu <- uu[uu$Visits!=0,]
uu$dayofweek <- weekdays(uu$fdate)

uu$extreme <- abs(uu$TEMP-mean(uu$TEMP,na.rm=TRUE)x)
cache(uu)