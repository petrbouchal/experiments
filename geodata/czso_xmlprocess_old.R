library(xml2)
library(xmlview)
library(listviewer)
library(htmltidy)
library(purrr)

budurl <- "http://report.czso.cz/reports/rwservlet?popisso&p_sort=1&p_idso=114812&p_datpohl=01032017&desformat=XML&mimetype=text/plain%0d%0aContent-disposition:attachment;filename=%22popisso_114812.xml%22"
hh <- read_xml(budurl)

hhh <- xml_find_all(hh, "//list_budovy") %>% 
  xml_children() %>%
  as_list() %>% 
  map_at(1:5, flatten) %>%
  map_at(1:5, unlist)

df_bud <- as.data.frame(hhh[1:5])

for(i in 1:length(hhh)) {
  prvek <- hhh[i]
  pocetadres <- length(hhh[[3]][names(hhh[[1]])=="adresy"])
  row_bud <- data.frame()
  
}
