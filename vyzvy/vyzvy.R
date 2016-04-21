library(readxl)
library(dplyr)
library(tidyr)

vyzvy <- read_excel("~/Dropbox/MMR/vyzvy/Přehled všech výzev_20_4_16.xlsx")
zmeny <- read_excel("~/Dropbox/MMR/vyzvy/Rozdíl mezi datem vyhlášení a modifikací výzvy.xlsx")

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

dv <- vyzvy %>% select(opnum, opname, vyzvakod, vyzvanazev, druh, hodnoceni,
                       plandatumvyhl,alokaceCZK, stav, finalizovano)

vv <- zmeny %>% full_join(vyzvy, by="vyzvakod")
table(zmeny$stavZ)

vyzvyzaprogram <- dv %>%
  group_by(opname) %>%
  summarise(pocet=n())

zmenenevyzvy <- zmeny %>%
  select(opname, opabb, vyzvakod) %>%
  group_by(opname, vyzvakod) %>%
  mutate(pocetzmen=n(), zmenena=TRUE)

dvyzvy <- dv %>% 
  filter(plandatumvyhl < Sys.time() & opnum != 30 & finalizovano==T) %>% 
  left_join(zmenenevyzvy, by="vyzvakod") %>% 
  replace_na(list(zmenena=FALSE, pocetzmen=0))

library(ggplot2)

graphable <- dvyzvy %>% group_by(opname.x, zmenena, pocetzmen) %>% 
  summarise(castka = sum(alokaceCZK), pocet=n())

ggplot(graphable, aes(opabb, castka)) +
  geom_bar(aes(fill=pocetzmen), stat = "identity") +
  coord_flip()

ggplot(dvyzvy, aes(plandatumvyhl, pocetzmen, size=alokaceCZK)) + geom_point() + facet_wrap(~ opname.x)

ggplot(zmeny, aes(datumvyhl)) + geom_bar()
ggplot(zmeny, aes(rozdil)) + geom_bar()
ggplot(zmeny, aes(datumvyhl)) + geom_bar() + facet_wrap(~ opabb)
ggplot(zmeny, aes(rozdil)) + geom_bar() + facet_wrap(~ opabb)

ggplot(dvyzvy, aes(plandatumvyhl, opname.x)) + geom_point(aes(size=pocetzmen), alpha=.5)
