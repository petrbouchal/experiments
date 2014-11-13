library(reshape2)
library(dplyr)

anscombe_l <- melt(anscombe)
anscombe_l$var <- substr(anscombe_l$variable,1,1)
anscombe_l$chart <- substr(anscombe_l$variable,2,2)
anscombe_l$variable <- NULL

anscombe_l <- anscombe_l %>%
  group_by(chart) %>%
  mutate(obsid = row_number())


anscombe_fin <- dcast(anscombe_l, chart + obsid ~ var)
anscombe_fin <- anscombe_fin[complete.cases(anscombe_fin),]


library(ggplot)

ggplot(anscombe_fin, aes(var))