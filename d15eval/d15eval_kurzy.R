setwd("~/github/experiments/d15eval/")
source("./d15eval_setup.R")
setwd("~/github/experiments/d15eval/")
source("./d15eval_kurzy_setup.R")

kurzy3 <- kurzy_long %>% 
  filter(variable %in% dnyvtydnu,
         !is.na(kurz)) %>% 
  mutate(value=as.numeric(value),
         variable = droplevels(variable),
         variable = factor(variable, levels=dnyvtydnu))

loadcustomthemes(fontfamily = "Gill Sans MT")

# Hodnoceni kurzu ####

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

theme_discover <- theme(panel.grid.minor=element_line(color="grey96", size=.5),
                        panel.grid.major=element_line(color="white", size=.75),
                        strip.text = element_text(size=12, color="grey20"),
                        panel.background = element_rect(fill="grey96"),
                        axis.text.x = element_text(size=11),
                        strip.background=element_rect(fill="grey96"))

ggplot(kurzyplot, aes(x=variable, y=value, group=kurzslot, colour=kurzslot)) +
  geom_line(size=1) +
  geom_point(size=3, pch=19) +
  facet_wrap(~ kurz) + 
  scale_y_continuous(limits=c(1,3)) +
  scale_colour_manual(values=c("lightgoldenrod1","aquamarine3","black")) +
  geom_text(aes(label=formatC(value, digits=2, format="f"),
                y=value+.2),
            size=3, colour="grey40",fontface="bold",
            data=kurzyplot[kurzyplot$kurzslot=="oba",]) +
  theme_discover

##  Hodnoceni lektora #####

lektor <- kurzy_long %>%
  filter(grepl("kurzobsah_",.$variable), !is.na(kurz)) %>% 
  mutate(variable = droplevels(variable)) %>% 
  group_by(kurz, variable, value) %>% 
  summarise(pocet = n())

lektor_wide <- dcast(lektor, "... ~ value")

lektor$variable <- plyr::revalue(lektor$variable,
                                 c("kurzobsah_lekodbornik"="Byl lektor dostatečný odborník na tento kurz?",
                                   "kurzobsah_lekpoutave"="Učil lektor poutavě?",
                                   "kurzobsah_lekpripraveny"="Byl lektor dobře připravený na tento kurz?",
                                   "kurzobsah_lekpristupny"="Byl lektor přístupný i mimo setkání kurzu?",
                                   "kurzobsah_lekvysvet"="Vysvětloval lektor srozumitelně?",
                                   "kurzobsah_lekzapoj"="Vytvořil ti lektor prostor zapojit se do dění v kurzu?",
                                   "kurzobsah_prinos"="Byl kurz pro tebe celkově přínosný?",
                                   "kurzobsah_problemzapojit"="Měl/a jsi problém zapojit se do dění v kurzu?"
                                 ))
negativnihodnoty <- c("Určitě ne", "Spíše ne")
lektor2 <- lektor %>%  
  mutate(share=pocet/sum(pocet),
         share = ifelse(value %in% negativnihodnoty, -share, share),
         share = ifelse(value=="Nevím", -share/2, share)) %>% 
  filter(!is.na(variable))

addthis <- lektor2[lektor2$value=="Nevím",]
addthis$share <- -addthis$share
lektor2 <- rbind(lektor2, addthis)

lektor2 <- lektor2[!is.na(lektor2$variable),]
lektor2$variable <- droplevels(lektor2$variable)

lektor2$value <- ordered(lektor2$value)

l_positive <- lektor2[lektor2$share>0,]
l_negative <- lektor2[lektor2$share<0,]

l_negative$value <- ordered(l_negative$value, levels = levels(l_negative$value))

l_positive <- l_positive[!is.na(l_positive$variable),]
l_negative <- l_negative[!is.na(l_negative$variable),]

l_positive <- arrange(l_positive, value)
l_negative <- arrange(l_negative, desc(value))

ggplot() +
  aes(fill=value, x=variable, y=share, group=kurz, order=value) +
  geom_bar(stat="identity", position="stack", color="grey96",
           data=l_positive) +
  geom_bar(stat="identity", position="stack", color="grey96",
           data=l_negative) +
  facet_wrap(~kurz, nrow = 6) + 
  coord_flip() +
  # scale_fill_brewer(palette = "RdBu") +
  scale_fill_manual(values=c("Určitě ne"="firebrick3",
                             "Spíše ne"="firebrick1",
                             "Nevím"="grey95",
                             "Spíše ano"="steelblue1",
                             "Určitě ano"="steelblue3"),
                    breaks=c("Určitě ne","Spíše ne","Nevím","Spíše ano",
                             "Určitě ano")) +
  scale_y_continuous(labels=percent) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_discover + theme(panel.grid.major.y=element_blank(),
                         panel.grid.major.x=element_line(color="white"),
                         axis.text.y=element_text(size=12))

## Velikost x hodnocení ####

setwd("~/github/experiments/d15eval/")
velikost <- read.csv("velikostkurzu.csv")
velikost <- melt(velikost, id.vars="kurz")
velikost <- plyr::rename(velikost,c("variable"="kurzslot", "value"="velikost")) 

kurzhodavg <- kurzy_long %>% 
  select(kurzslot, variable, value, kurz) %>% 
  filter(variable %in% dnyvtydnu) %>%
  group_by(kurz, kurzslot) %>% 
  summarise(value=mean(as.numeric(value), na.rm=T)) %>% 
  merge(velikost)

ggplot(kurzhodavg, aes(velikost, value, color=kurzslot)) +
  geom_point()

kurzhodavg_c <- kurzhodavg[complete.cases(kurzhodavg),]

cor(kurzhodavg_c$value, kurzhodavg_c$velikost)

## Korelace

m <- select(kurzy_long, kurzslot, value)
m <- m[complete.cases(m),]
m$value <- as.numeric(m$value)

t.test(m$value~m$kurzslot)

hod <- filter(kurzy_long, variable %in% dnyvtydnu)
hod$value <- as.numeric(hod$value)

model <- lm(value ~ variable + rocnik + pohlavi + kurzslot + kurz, data = hod)
summary(model)

library(sjPlot)
sjp.lm(model, sort.est = F)

rm("addthis", "hod", "kurzhodavg", "kurzhodavg_c", "kurzy2", "kurzy3",
   "kurzy4", "kurzy5", "lektor2")

## Didaktika ####

## Náročnost ####

negativnihodnoty <- c("Velmi těžké","Spíše těžké")

narocnost <- kurzy_long %>% 
  filter(grepl("narocnost",variable)) %>% 
  group_by(kurz, variable, value) %>%
  summarise(pocet=n()) %>% 
  group_by(kurz, variable) %>% 
  mutate(share=pocet/sum(pocet)) %>% 
  mutate(share = ifelse(value %in% negativnihodnoty,-share, share),
         share = ifelse(value=="Vhodně náročné",-share/2, share)) %>%
  filter(!is.na(value)) %>% 
  filter(value!="Mého kurzu se netýká")
  filter(!is.na(variable))  

addthis <- narocnost[narocnost$value=="Vhodně náročné",]
addthis$share <- -addthis$share
narocnost <- rbind(narocnost, addthis)

narocnost$value <- ordered(narocnost$value)
l_positive <- narocnost[narocnost$share>0,]
l_negative <- narocnost[narocnost$share<0,]

l_negative$value <- ordered(l_negative$value, levels = levels(l_negative$value))

l_positive <- l_positive[!is.na(l_positive$variable),]
l_negative <- l_negative[!is.na(l_negative$variable),]

l_positive <- arrange(l_positive, value)
l_negative <- arrange(l_negative, desc(value))

ggplot() +
  aes(fill=value, x=variable, y=share, group=kurz, order=value) +
  geom_bar(stat="identity", position="stack", color="grey96",
           data=l_positive) +
  geom_bar(stat="identity", position="stack", color="grey96",
           data=l_negative) +
  facet_wrap(~kurz, nrow = 6) + 
  coord_flip() +
  # scale_fill_brewer(palette = "RdBu") +
  scale_fill_manual(values=c("Určitě ne"="firebrick3",
                             "Spíše ne"="firebrick1",
                             "Nevím"="grey95",
                             "Spíše ano"="steelblue1",
                             "Určitě ano"="steelblue3"),
                    breaks=c("Určitě ne","Spíše ne","Nevím","Spíše ano",
                             "Určitě ano")) +
  scale_y_continuous(labels=percent) +
  guides(fill = guide_legend(override.aes = list(colour = NULL))) +
  theme_discover + theme(panel.grid.major.y=element_blank(),
                         panel.grid.major.x=element_line(color="white"),
                         axis.text.y=element_text(size=12))

  
  
