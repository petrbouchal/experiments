pp <- read.csv("./projektyESIF.tsv", sep = "\t", fileEncoding = "UTF8")

library(pbtools)
loadcustomthemes()

pp <- pp %>% 
  melt(id.vars="Rok") %>% 
  group_by(variable) %>% 
  mutate(total=sum(value)) %>% 
  mutate(Rok=ifelse(Rok==2015,"Celkem",Rok)) %>% 
  group_by(Rok) %>% 
  mutate(maximum=value==max(value), maxtotal=total==max(total))


ggplot(pp, aes(variable, value, group=Rok)) +
#   geom_bar(aes(y=total), stat = "identity", fill="darkgrey", data=pp[pp$Rok=="Celkem",]) +
#   geom_bar(aes(y=value), stat = "identity", fill="blue") +
  geom_line(aes(y=total), colour="darkgrey", data=pp[pp$Rok=="Celkem",]) +
  geom_line(aes(y=value), colour="blue") +
  facet_wrap(~Rok)

ggplot(pp, aes(variable, value, group=Rok)) +
  geom_bar(aes(y=total, fill=maxtotal), stat = "identity", data=pp[pp$Rok=="Celkem",]) +
  geom_bar(aes(y=value, fill=maximum), stat = "identity") +
  scale_x_discrete(breaks=c("Leden","Duben","Červenec","Říjen")) + 
  facet_wrap(~Rok)