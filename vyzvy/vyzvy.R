library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(stringr)

# vyzvy <- read_excel("~/Dropbox/MMR/vyzvy/Přehled všech výzev_20_4_16.xlsx")
# zmeny <- read_excel("~/Dropbox/MMR/vyzvy/Rozdíl mezi datem vyhlášení a modifikací výzvy.xlsx")
vyzvy <- read_excel("D:/usr/Dropbox/MMR/vyzvy/Přehled všech výzev_20_4_16.xlsx")
zmeny <- read_excel("D:/usr/Dropbox/MMR/vyzvy/Rozdíl mezi datem vyhlášení a modifikací výzvy.xlsx")
zaheslovani <- read_excel("D:/usr/Dropbox/MMR/vyzvy/zaheslování výzev.xlsx")

names(zmeny) <- zmeny[2,]
zmeny <- zmeny[-c(1,2),]

names(zmeny) <- c("opabb","opname","vyzvakod","stavZ","datumvyhl","datumzmenyZ",
                  "rozdil")
names(vyzvy) <- c("opnum","opname","vyzvakod","vyzvanazev","platna","druh",
                  "hodnoceni","plandatumvyhl","plandatumprijem","plandatumukonpredbez",
                  "plandatumukonzadosti",
                  "alokaceCZK","alokaceEUR","aktivity","cilovky","uzemi",
                  "typprijemce","finalizovano","menaalokace","alokaceEU","stav",
                  "alokoaceNAR",
                  "datumzmeny","komplementarita","synergie","verzeharmonogramu",
                  "autor","vyzvanazevprozadatele")

names(zaheslovani) <- c("opabb","vyzvakod","vyzvanazev")
zaheslovani <- zaheslovani[-c(1,2),] %>% 
  filter(str_detect(vyzvakod,"[0-9]{2}_{1}"))

nametable <- zmeny %>% group_by(opabb, opname) %>% summarise() %>% filter(!is.na(opabb))

dv <- vyzvy %>% select(opnum, opname, vyzvakod, vyzvanazev, druh, hodnoceni,
                       plandatumvyhl,alokaceCZK, stav, finalizovano)

vv <- zmeny %>% full_join(vyzvy, by="vyzvakod")

vyzvyzaprogram <- dv %>%
  group_by(opname) %>%
  summarise(pocet=n())

zmenenevyzvy <- zmeny %>%
  select(opname, vyzvakod) %>%
  group_by(opname, vyzvakod) %>%
  mutate(pocetzmen=n(), zmenena=TRUE)

dvyzvy <- dv %>% 
  filter(plandatumvyhl < Sys.time() & opnum != 30 & finalizovano==T) %>% 
  left_join(zmenenevyzvy, by="vyzvakod") %>% 
  replace_na(list(zmenena=FALSE, pocetzmen=0)) %>% 
  left_join(nametable, by=c("opname.x"="opname")) %>%  
  mutate(opabb=ifelse(opname.x=="Operační program Rybářství","OP R", opabb),
         opabb=ifelse(opname.x=="Program rozvoje venkova","PRV", opabb),
         oplabel=paste(opnum,opabb),
         zaheslovana = ifelse(vyzvakod %in% zaheslovani$vyzvakod, TRUE, FALSE)) %>% 
  arrange(opnum)

dvyzvy <- dvyzvy %>% 
  mutate(opabb = factor(opabb, ordered=T,
                        levels=unique(dvyzvy[order(dvyzvy$opnum),"opabb"][[1]])))

graphable <- dvyzvy %>%
  group_by(opname.x, zmenena, pocetzmen, opabb, oplabel) %>%
  filter(!(opabb %in% c("PRV","OP R"))) %>% 
  summarise(castka = sum(alokaceCZK), pocet=n())

library(ggplot2)

theme_vyzvymulti <- theme_minimal() +
  theme(strip.text=element_text(size=12, face="bold"),
        plot.title=element_text(hjust=0, face="bold"),
        strip.background=element_rect(fill="gray92", colour=NA),
        panel.background=element_rect(fill="gray92", colour=NA),
        panel.grid.major=element_line(colour="white"),
        panel.grid.minor=element_blank(),
        plot.margin=unit(c(20,10,10,10),"points"),
        axis.ticks=element_blank(),
        panel.margin.x=unit(10,"points"),
        panel.margin.y=unit(10,"points"),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        axis.text.y=element_text(hjust = 1),
        axis.text.x=element_text(vjust = 1),
        legend.box.just="left",
        legend.box="horizontal")

theme_vyzvy <- theme_minimal() +
  

ggplot(dvyzvy[!(dvyzvy$opabb %in% c("PRV","OP R")),],
       aes(as.Date(plandatumvyhl), pocetzmen, size=alokaceCZK/1000000)) +
  geom_jitter(alpha=.5, pch=19, aes(colour=zaheslovana),
              position=position_jitterdodge(jitter.width = 50, jitter.height = 0, dodge.width = 0.75)) +
  facet_wrap(~ opabb) + theme_vyzvymulti +
  theme(legend.position="bottom", legend.direction="horizontal") +
  scale_size(labels=comma, breaks = c(0, 5000,10000,20000,30000)) + 
  scale_colour_discrete(labels=c("Ano","Ne"), breaks=c(TRUE, FALSE)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y") +
  labs(size="Alokace (mil. CZK)", label="Modifikace výzev", colour="Použito zaheslování") + 
  xlab("Datum vyhlášení výzvy") +
  ylab("Počet modifikací výzvy") +
  ggtitle("Modifikace výzev podle objemu",
          subtitle = "Finalizované výzvy vyhlášené do 20.4.2016")

ggplot(graphable, aes(opabb, castka)) +
  geom_bar(aes(fill=pocetzmen), stat = "identity") +
  coord_flip() + theme_vyzvymulti + xlab(NULL)

ggplot(zmeny[!is.na(zmeny$opabb),], aes(datumvyhl)) +
  geom_bar()

ggplot(zmeny[!is.na(zmeny$opabb),], aes(rozdil)) +
  geom_bar()

ggplot(zmeny[!is.na(zmeny$opabb),], aes(datumvyhl)) +
  geom_bar() +
  facet_wrap(~ opabb) +
  theme_vyzvymulti() + 
  

ggplot(zmeny[!is.na(zmeny$opabb),], aes(rozdil)) + geom_bar() + facet_wrap(~ opabb) + theme_vyzvymulti()

ggplot(dvyzvy, aes(plandatumvyhl, opname.x)) + geom_point(aes(size=pocetzmen), alpha=.5) + theme_vyzvymulti()
