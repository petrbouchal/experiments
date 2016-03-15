library(ggiraph)
mytheme_main <- theme( panel.background = element_blank(), 
                       panel.grid.major = element_line(colour = "#dddddd"), 
                       axis.ticks = element_line(colour = "#dddddd") )

mytheme_map <- theme(
  panel.background = element_blank(), axis.title.x = element_blank(),
  axis.text = element_blank(), axis.line.x = element_blank(),
  axis.line.y = element_blank(), axis.title.y = element_blank(),
  axis.ticks.x = element_blank(), axis.ticks.y = element_blank() )

dataset <- mtcars
dataset$tooltip <- row.names(dataset)

gg_point_1 <- ggplot(dataset, aes(x = disp, y = qsec, 
                                  color = wt, tooltip = tooltip ) ) + 
  geom_point_interactive(size=3)

ggiraph(code = {print(gg_point_1 + mytheme_main)}, width = 7, height = 6)

dataset$data_id <- tolower(row.names(dataset))

gg_point_2 <- ggplot(dataset, aes(x = disp, y = qsec, 
                                  color = wt, tooltip = tooltip, data_id = data_id ) ) + 
  geom_point_interactive(size=4)

# htmlwidget call
ggiraph(code = {print(gg_point_2 + mytheme_main)}, 
        width = 7, height = 6, 
        hover_css = "fill:orange;r:6px;")

# with subtitle, depends on https://github.com/hadley/ggplot2/pull/1582
ggiraph(code={print(ggplot(data=mtcars, aes(mpg, cyl)) + geom_point_interactive() + labs(title="blah", subtitle="blah blah", caption="blahnotation"))})

