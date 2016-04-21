library(dplyr)
library(ggplot2)
library(scales)

balance <- data.frame(desc = c("Starting Cash","Sales", "Refunds", "Payouts", "Court Losses",
                               "Court Wins", "Contracts", "End Cash"),
                      amount = c(2000,3400, -1100, -100, -6600, 3800, 1400, 2800))

d <- gsub(" ", "\n", c("Starting Cash", "Sales", "Refunds", "Payouts", "Court Losses", "Court Wins", "Contracts", "End Cash"))

balance <- balance %>%
  "inc", "dec")),
  end = ifelse(desc != last(desc), cumsum(amount), last(amount)),
  start = ifelse(desc != last(desc), lag(end, default = 0), 0),
  vjust = ifelse(type != "dec", -0.3, 1.2))

ggplot(balance, aes(fill=type)) +
  geom_rect(aes(x=desc, xmin=id - 0.4, xmax=id + 0.4, ymin=end, ymax=start)) +
  geom_segment(data = head(balance, -1), size=1, aes(x=id - 0.4, xend=id + 1.4, y=end, yend=end)) +    
  geom_text(vjust=balance$vjust, fontface="bold", size=6, aes(x=id, y=end, label=comma(amount), color=type)) +
  scale_color_manual(values=c(dec="indianred", inc="forestgreen", net="dodgerblue2")) +      
  theme(legend.position="none", axis.text.x=element_text(size = 18), axis.text.y=element_text(size = 17), plot.title=element_text(face="bold", size=20)) + 
  labs(x="", y="")