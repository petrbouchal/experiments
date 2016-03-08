library(readxl)
library(ggplot2)
library(ggiraph)
library(lubridate)
library(dplyr)
library(Cairo)
library(gganimate)
library(animation)
library(ggiraph)

# v <- read_excel("~/Dropbox/MMR/Admin/Výplatní pásky/vyplaty_data.xlsx")
v <- read_excel("D:/usr/Dropbox/MMR/Admin/Výplatní pásky/vyplaty_data.xlsx")
v <- v %>%
  mutate(date=as.POSIXct(strptime(paste(year,month,"01",sep="-"), format="%Y-%m-%d")),
         tooltip=paste(month.name[month],year),
         grp=1) %>% 
  group_by(date, tooltip, grp) %>%
  select(date, tooltip, net, gross, paid) %>% 
  summarize_each("sum")

gg_point_1 <- ggplot(v, aes(x = date, y = net, tooltip = tooltip, group=grp) ) + 
  geom_line() +
  geom_point_interactive(size=3)

library(plotly)
ggplotly(gg_point_1)

print(gg_point_1)

ggiraph(code = {print(gg_point_1 + mytheme_main)}, width = 7, height = 6)


gg_point_2 <- ggplot(v, aes(x = date, y = net, frame = date, group=grp)) + 
  geom_line(aes(group=grp, cumulative=T)) +
  geom_point(size=3)

gg_animate(gg_point_2, "output.gif")
gg_animate(gg_point_2, "output.html", ani.options=ani.options(loop=F, ani.dev=Cairo))

png(type="cairo-png",res = 227, width = 1000, height=1000, units = "px",
    antialias = "gray", pointsize = 10)
gg_point_2
dev.off()

library(svglite)
svglite()
gg_point_2
dev.off()
