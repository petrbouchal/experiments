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

ggplot(kurzyplot, aes(x=variable, y=value, group=kurzslot, colour=kurzslot)) +
  geom_line(size=1) +
  geom_point(size=3, pch=19) +
  facet_wrap(~ kurz) + 
  scale_y_continuous(limits=c(1,5)) +
  scale_colour_manual(values=c("lightgoldenrod1","aquamarine3","black")) +
  geom_text(aes(label=formatC(value, digits=2, format="f"),
                y=value+.5),
            size=3, colour="black",fontface="bold",
            data=kurzyplot[kurzyplot$kurzslot=="oba",]) +
  theme(strip.background=element_rect(fill="darkgrey"),
        panel.grid.minor=element_line(color="white", size=.5),
        panel.grid.major=element_line(color="white", size=.75),
        strip.text = element_text(size=12, color="white"),
        panel.background = element_rect(fill="grey96"),
        axis.text.x = element_text(size=11))

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

lektor$value <- as.factor(lektor$value)
levels(lektor$value)
lektor$value <- factor(lektor$value, levels(lektor$value)[c(5,3,1,4,2)])
levels(lektor$value)

lektor <- arrange(lektor, value)

ggplot(lektor, aes(group=kurz, x=variable, y=pocet, fill=value)) +
  geom_bar(stat="identity", position="fill", color="white") + 
  facet_wrap(~ kurz) + 
  coord_flip() +
  scale_fill_brewer(palette = "RdBu") +
  scale_y_continuous()

# Home-made Likert thing

lektor <- kurzy_long %>%
  filter(grepl("kurzobsah_",.$variable), !is.na(kurz)) %>% 
  mutate(variable = droplevels(variable)) %>% 
  group_by(kurz, variable, value) %>% 
  summarise(pocet = n())

lektor2 <- lektor %>%  
  mutate(total=sum(pocet),
         share=pocet/total,
         share = ifelse(value=="Určitě ne",-share, share),
         share = ifelse(value=="Spíše ne",-share, share),
         share = ifelse(value=="Nevím",-share/2, share))

addthis <- lektor2[lektor2$value=="Nevím",]
addthis$share <- -addthis$share
lektor2 <- rbind(lektor2, addthis)

lektor2 <- lektor2[!is.na(lektor2$variable),]
lektor2$variable <- droplevels(lektor2$variable)

l_positive <- lektor2[lektor2$share>0,]
l_positive$value <- as.factor(l_positive$value)
levels(l_positive$value)
l_negative <- lektor2[lektor2$share<0,]
l_negative$value <- as.factor(l_negative$value)
levels(l_negative$value)

ggplot() +
  geom_bar(stat="identity", position="stack", color=NA,
           data=l_positive,
           aes(order=-as.numeric(value),fill=value,
               x=variable,y=share,group=kurz)) +
  geom_bar(stat="identity", position="stack", color=NA,
           data=l_negative, 
           aes(order=as.numeric(value),fill=value,
               x=variable, y=share, group=kurz)) +
  facet_wrap(~kurz) + 
  coord_flip() +
  scale_fill_brewer(palette = "RdBu")

# Likert - using likert package, grouping is not working

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

## Didaktika ####

## Náročnost ####

narocnost <- kurzy_long %>% 
  filter(grepl("narocnost",variable)) %>% 
  group_by(kurz, variable, value) %>%
  summarise(pocet=n()) %>% 
  group_by(kurz, variable) %>% 
  mutate(share=pocet/sum(pocet))
  

  
  
