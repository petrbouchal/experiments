library(xml2)
library(httr)
library(leaflet)
library(htmltools)
library(widgetframe)
library(tidyverse)

library(xmlview)
library(listviewer)
library(htmltidy)

# https://rud.is/rpubs/xml2power/

doc <- read_xml("~/Downloads/popisso_114812.xml")

xtrct <- function(doc, target) { xml_find_all(doc, target) %>% xml_text() %>% trimws() }

xtrct_df <- function(doc, top) {
  xml_find_first(doc, sprintf(".//%s", top)) %>%
    xml_children() %>%
    xml_name() %>%
    map(~{
      xtrct(doc, sprintf(".//%s/%s", top, .x)) %>%
        list() %>%
        set_names(tolower(.x))
    }) %>%
    flatten()
}

adresy_df <- xtrct_df(doc, "adresy")
adresy_filtered <- sapply(adresy_df, function(x) {if(length(x)==18) {x[which(adresy_df$typadr==1)]} else x})

budovy_df <- xtrct_df(doc, "budovy")
budovy_filtered <- sapply(budovy_df, function(x) {if(length(x)==18) {x[which(budovy_df$typadr==1)]} else x})

adresy_df <- as_data_frame(adresy_filtered)
budovy_df <- as_data_frame(budovy_filtered[1:5])
adresy_df$idob <- budovy_df$idob
adresy_df$cisdom <- budovy_df$cisdom
