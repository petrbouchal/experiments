Sys.setlocale(category = "LC_ALL", locale = "Czech_Czech Republic.1252")

library(pbtools)
library(dplyr)
library(reshape2)
library(tidyr)
library(Cairo)

loc <- "C:/Users/boupet/Downloads/eval"

## Load data using LimeSurvey script

setwd(loc)
getwd()
source("survey_317642_R_syntax_file.R")

## Kurzy: select vars and reshape into long format

kurzy <- data %>% 
  select(starts_with("kurz"),pohlavi,rocnik,vek, -kurzyjakedalsi, id ,-contains("Time")) %>%
  melt(id.vars=c("id","pohlavi","vek","rocnik"))

kurzy$kurzslot <- ifelse(grepl("kurz1",kurzy$variable),"A","B")
  
## Mark course slot and harmonise variable names so ratings from each course
## have the same variable code regardless of slot

kurzy$variable <- gsub("kurz1","kurz",kurzy$variable)
kurzy$variable <- gsub("kurz2","kurz",kurzy$variable)

## Create day markers for rating variables

kurzy$variable[kurzy$variable=="kurzhodnoceni_1"] <- "pondělí"
kurzy$variable[kurzy$variable=="kurzhodnoceni_2"] <- "úterý"
kurzy$variable[kurzy$variable=="kurzhodnoceni_3"] <- "středa"
kurzy$variable[kurzy$variable=="kurzhodnoceni_4"] <- "čtvrtek"
kurzy$variable[kurzy$variable=="kurzhodnoceni_5"] <- "pátek"
kurzy$variable[kurzy$variable=="kurzhodnoceni_6"] <- "sobota"

dnyvtydnu <- c("pondělí","úterý","streda","ctvrtek","patek","sobota")

## chart for each course by day

kurzy2 <- dcast(kurzy, "... ~ variable", value.var="value")

kurzy_long <- melt(kurzy2, id.vars=c("id","pohlavi","vek","rocnik","kurzktery","kurzslot"))
kurzy_long <- plyr::rename(kurzy_long, c("kurzktery"="kurz"))

kurzy3 <- kurzy_long %>% 
  filter(variable %in% dnyvtydnu,
         !is.na(kurz)) %>% 
  mutate(value=as.numeric(value),
         variable = droplevels(variable),
         variable = factor(variable, levels=dnyvtydnu))

loadcustomthemes(fontfamily = "Calibri")

# Hodnoceni kurzu

kurzy4 <- kurzy3 %>% 
  group_by(variable, kurz, kurzslot) %>% 
  summarise(value = mean(value, na.rm = T))

kurzy5 <- kurzy3 %>% 
  group_by(variable, kurz) %>% 
  summarise(value = mean(value, na.rm = T))

kurzy5$kurzslot <- "oba"
kurzyplot <- rbind(kurzy4, kurzy5)

kurzyplot <- kurzyplot %>% 
  group_by(kurz, variable) %>% 
  mutate(pocetslotu = n()-1)

ggplot(kurzyplot, aes(x=variable, y=value, group=kurzslot, colour=kurzslot)) +
  geom_line(size=1) +
  geom_point(size=3, pch=19) +
  facet_wrap(~ kurz) + 
  scale_y_continuous(limits=c(1,5))

##  Hodnoceni lektora

lektor <- kurzy_long %>%
  filter(grepl("kurzobsah_",.$variable), !is.na(kurz)) %>% 
  mutate(variable = droplevels(variable)) %>% 
  group_by(kurz, variable, value, ) %>% 
  summarise(pocet = n())

lektor <- dcast(lektor, "... ~ value")

ggplot(lektor, aes(group=kurz, x=variable, y=pocet, fill=value)) +
  geom_bar(stat="identity", position="fill") + 
  facet_wrap(~ kurz) + 
  coord_flip()

# Likert

ldata <- kurzy_long %>% 
  filter(grepl("kurzobsah_",.$variable), !is.na(kurz)) %>% 
  select(kurz, value, variable, id)

ldata <- dcast(ldata, "id + ... ~ variable")
ldatakurz <- as.factor(ldata$kurz)
ldata <- select(ldata, starts_with("kurzobsah_"))

ldata <- as.data.frame(
  lapply(ldata, function (x) factor(x,
                                    levels = c("Urcite ano","Spíše ano",
                                               "Nevím", "Spíše ne", "Urcite ne"))))
likertobject <- likert(items=ldata, grouping = ldatakurz)
likert.bar.plot(likertobject)
