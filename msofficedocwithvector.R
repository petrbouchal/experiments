install.packages("ReporteRs")

normal_text = c("This is a first text.", "This is a second text.")
bullet_text = c("This is a first list item.", "This is a second list item.")
library( "ggplot2" )
myplot = qplot(Sepal.Length, Petal.Length, data = iris,
color = Species, size = Petal.Width, alpha = I(0.7) )
head( mtcars )
library( "ReporteRs" )
library( "magrittr" )
doc_obj = docx( title = "title", template = "input/word-template.docx" )
doc_obj = docx( title = "title", template = "word-template.docx" )
styles( doc_obj )
doc_obj = doc_obj %>%
addParagraph( value = normal_text, stylename="Normal" ) %>%
addParagraph( value = bullet_text, stylename="bullet-list" )
doc_obj = doc_obj %>%
addParagraph( value = "my first plot", stylename = "plot-caption") %>%
addPlot( fun = print, x = myplot, width = 4, height = 4, par.properties = parCenter(), vector.graphic = TRUE )
writeDoc( doc_obj, file = "my_document.docx")
