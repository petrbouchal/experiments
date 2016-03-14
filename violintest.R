vd <- read.csv("violintest.tsv")

library(ggplot2)

ggplot(vd, aes(kurz,znamka, group=kurz)) + 
  geom_violin(colour=NA, aes(fill=kurz)) +
  geom_point(position = position_jitter(width=0)) +
  facet_wrap(~ turnus)

# two-sided asymmetrical violin?