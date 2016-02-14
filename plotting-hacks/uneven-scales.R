ap <- AirPassengers[[1]]
plot(AirPassengers)

xticks <- sort(c(min(AirPassengers),max(AirPassengers),AirPassengers[length(AirPassengers)]))
xticks_char <- paste0(as.character(xticks),"       ")

xticks2 <- sort(c(AirPassengers[1],max(AirPassengers),AirPassengers[length(AirPassengers)]))
xticks2_char <- paste0(as.character(xticks2),"       ")

nticks <- seq(0, 650, 50)

ticks <- sort(c(nticks,xticks))
ticks_char <- sort(c(xticks_char,as.character(nticks)))

ticks2 <- sort(c(nticks,xticks2))
ticks2_char <- sort(c(xticks2_char,as.character(nticks)))

library(ggplot2)

ap <- data.frame(Y=as.matrix(AirPassengers), date=time(AirPassengers))
ggplot(ap, aes(date, Y)) +
  geom_line() +
  scale_y_continuous(breaks=ticks2, labels=ticks2_char)
