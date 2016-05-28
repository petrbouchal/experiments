library(readxl)
library(dplyr)
library(tidyr)
library(scales)
library(stringr)
library(extrafont)

loadfonts()

thousandspace <- function(l) {
  format(l, big.mark = " ", scientific = F)
}

vyzvy <- read_excel("~/Dropbox/MMR/vyzvy/Přehled všech výzev_20_4_16.xlsx")
zmeny <- read_excel("~/Dropbox/MMR/vyzvy/Rozdíl mezi datem vyhlášení a modifikací výzvy.xlsx")
zaheslovani <- read_excel("~/Dropbox/MMR/vyzvy/zaheslování výzev.xlsx")
# vyzvy <- read_excel("D:/usr/Dropbox/MMR/vyzvy/Přehled všech výzev_20_4_16.xlsx")
# zmeny <- read_excel("D:/usr/Dropbox/MMR/vyzvy/Rozdíl mezi datem vyhlášení a modifikací výzvy.xlsx")
# zaheslovani <- read_excel("D:/usr/Dropbox/MMR/vyzvy/zaheslování výzev.xlsx")

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

nametable <- zmeny %>% group_by(opabb, opname) %>% summarise() %>%
  filter(!is.na(opabb)) %>% 
  left_join(vyzvy %>% group_by(opnum,opname) %>% summarise())

dv <- vyzvy %>% select(opnum, opname, vyzvakod, vyzvanazev, druh, hodnoceni,
                       plandatumvyhl,alokaceCZK, stav, finalizovano, plandatumprijem,
                       plandatumukonpredbez, plandatumukonzadosti)

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
         zaheslovana = ifelse(vyzvakod %in% zaheslovani$vyzvakod, TRUE, FALSE)) %>%
  mutate(opabb = factor(opabb, ordered=T,
                        levels=unique(.[order(.$opnum.x),"opabb"][[1]])))

zmeny <- zmeny %>%
  left_join(nametable) %>% 
  mutate(opabb = factor(opabb, ordered=T,
                        levels=unique(.[order(.$opnum),"opabb"][[1]])))

graphable <- dvyzvy %>%
  group_by(opname.x, zmenena, pocetzmen, opabb) %>%
  filter(!(opabb %in% c("PRV","OP R"))) %>% 
  summarise(castka = sum(alokaceCZK), pocet=n())

library(ggplot2)

theme_vyzvymulti <-
  theme(text=element_text(),
        strip.text=element_text(size=12, face="bold", colour="grey50",
                                hjust = 0, margin=margin(15,5,5,5,"points")),
        plot.title=element_text(hjust=0, face="bold"),
        plot.caption=element_text(lineheight = .5, face="italic",size=10),
        strip.background=element_rect(fill="gray93", colour=NA),
        panel.background=element_rect(fill="gray93", colour=NA),
        panel.grid.major=element_line(colour="white", size = .3),
        panel.grid.minor.x=element_line(colour="grey96",size = .2),
        panel.grid.minor.y=element_blank(),
        plot.margin=unit(c(20,10,10,10),"points"),
        axis.ticks=element_blank(),
        panel.margin.x=unit(10,"points"),
        panel.margin.y=unit(10,"points"),
        axis.title.y=element_text(margin=margin(0,10,0,0)),
        axis.title.x=element_text(margin=margin(10,0,0,0)),
        legend.box.just="left",
        legend.key = element_rect(fill=NA, colour=NA),
        legend.box="horizontal",
        legend.background=element_rect(fill="gray93", colour=NA))

theme_vyzvy <- theme_minimal() +
  theme()

ggplot(dvyzvy[!(dvyzvy$opabb %in% c("PRV","OP R")),],
       aes(as.Date(plandatumvyhl), pocetzmen, size=alokaceCZK/1000000)) +
  geom_jitter(alpha=.5, pch=19, aes(colour=zaheslovana), stroke=0,
              position=position_jitterdodge(jitter.width = 50,
                                            jitter.height = 0,
                                            dodge.width = 0.75)) +
  facet_wrap(~ opabb) + theme_vyzvymulti +
  scale_size(labels=thousandspace, breaks = c(0, 5000,10000,20000,30000)) + 
  scale_colour_discrete(labels=c("Ano","Ne"), breaks=c(TRUE, FALSE)) +
  scale_x_date(date_breaks = "3 months", date_labels = "%m/%y", date_minor_breaks = "1 month") +
  labs(size="Alokace (mil. CZK)", label="Modifikace výzev",
       colour="Použito zaheslování", 
       caption="PRV a OP R vynechány - používají vlastní systém a není jisté,\n 
       jestli se údaje o mofidikacích přenáší do MSSF.") + 
  xlab("Datum vyhlášení výzvy") +
  ylab("Počet modifikací výzvy") +
  theme(legend.position="bottom", legend.direction="horizontal") +
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
  theme_vyzvymulti
  

ggplot(zmeny[!is.na(zmeny$opabb),], aes(rozdil)) + geom_bar() + facet_wrap(~ opabb) + theme_vyzvymulti

ggplot(dvyzvy, aes(plandatumvyhl, opname.x)) + geom_point(aes(size=pocetzmen), alpha=.5) + theme_vyzvymulti

dvv <- dvyzvy %>% mutate(vyzvakod=str_sub(vyzvakod, 4))
dvv$xend1 <- dvv$plandatumukonpredbez
dvv$xend2 <- dvv$plandatumukonzadosti
dvv$xend1[dvv$plandatumukonpredbez > as.POSIXct("2016-4-21")] <- as.POSIXct("2016-4-21")
dvv$xend2[dvv$plandatumukonzadosti > as.POSIXct("2016-4-21")] <- as.POSIXct("2016-4-21")
dvv$opabb <- paste0(" ", dvv$opabb)
zmeny$opabb <- paste0(" ", zmeny$opabb)
zmeny$vyzvakod <- str_sub(zmeny$vyzvakod, 4)

ggplot(data=dvv[!(dvv$opabb %in% c("PRV","OP R")),], aes(y=vyzvakod, yend=vyzvakod)) +
  geom_segment(aes(x=as.Date(plandatumvyhl), xend=as.Date(xend1),
               colour="grey50"), size=1.2) + 
  geom_segment(aes(x=as.Date(plandatumprijem),
                   xend=as.Date(xend2), colour="lightblue"),  size=1.4) + 
  geom_point(data=zmeny[(zmeny$vyzvakod %in% dvv$vyzvakod) & !(zmeny$opabb %in% c("PRV","OP R")),],
             aes(x=as.Date(datumzmenyZ), shape="Modifikace výzvy"),
             colour="red1", stroke=0) +
  facet_wrap(~opabb, scales = "free_y", nrow = 2) +
  scale_x_date(limits = c(as.Date("2015-6-1"),as.Date(Sys.Date())), date_labels = "%m/%y") +
  scale_colour_manual(values=c("grey50"="grey50","lightblue"="lightblue"),
                      guide="legend", name=NULL,
                      labels=c("Doba příjmu předběžných žádostí",
                               "Doba příjmu žádostí o podporu")) +
  scale_shape_manual(labels=c("Modifikace výzvy"), name=NULL, guide="legend",
                     values=c("Modifikace výzvy"=19)) +
  guides(shape="legend") +
  theme_vyzvymulti +
  theme(plot.background = element_rect(fill="grey92", colour = NA),
        strip.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        panel.grid = element_line(colour="grey92"),
        axis.text.y=element_text(size=5, hjust=-.5, margin = margin(0,0,0,0)),
        legend.position="bottom", legend.direction="horizontal") +
  labs(title="Vyhlášené výzvy a jejich modifikace",
       subtitle="všechny výzvy finalizované před 20. dubnem 2015",
       x=NULL, y=NULL,
       caption="PRV a OP R vynechány - používají vlastní systém a není jisté,\n 
       jestli se údaje o modifikacích přenáší do MSSF.")

