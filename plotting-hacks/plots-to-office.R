library(rvg)
library(ggplot2)

gg = ggplot( iris, aes(Sepal.Length, Sepal.Width, 
                       color = Petal.Width)) + geom_point()

write_docx(file = "my_plot.docx", code = print( gg ))
write_pptx(file = "my_plot.pptx", code = print( gg ))