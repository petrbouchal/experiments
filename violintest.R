vd <- read.csv("violintest.tsv")

library(ggplot2)

ggplot(vd, aes(kurz,znamka, group=kurz)) + 
  geom_violin() +
  geom_point(position = position_jitter(width=0)) +
  facet_wrap(~ turnus)
