library(readr)
library(foreign)
library(dplyr)
library(haven)

ess <- read.dta("~/Downloads/ESS6e02_1.dta")

table(ess$cntry)

library(ggplot2)

countries <- c("CZ","PL","DE","SK")

ess_subset <- ess %>%
  select(cntry, lrscale, hinctnta, pweight) %>% 
  filter(cntry %in% countries) %>% 
  filter(hinctnta != "No answer" & hinctnta != "Don't know" & hinctnta != "Refusal") %>%
  filter(lrscale != "No answer" & lrscale != "Don't know" & lrscale != "Refusal") %>%
  group_by(cntry) %>% 
  mutate(countrycases=n()) %>% 
  group_by(cntry, hinctnta, lrscale) %>% 
  summarise(countnum=sum(pweight)/mean(countrycases))

group_by(ess_subset, cntry) %>% summarise(total=sum(countnum))

ggplot(ess_subset, aes(lrscale, hinctnta)) +
  geom_point(aes(alpha=countnum)) +
  facet_wrap(~ cntry)
