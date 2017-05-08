library(tidyverse)
library(readxl)
library(foreign)
library(sf)
library(rvest)
library(stringr)

# priklad budovy http://apl.czso.cz/irso4/buddet.jsp?x=1&idob=1000153843&budid=685096

urls_from_SOcodes <- function(socode) {
  read_html(paste0("http://apl.czso.cz/irso4/budlist.jsp?kodcis=55&kod=", socode)) %>% 
    html_nodes(css = "a[title=\"Detailní údaje o budově\"]") %>% 
    html_attr(name = "href") %>%
    unique()
}

sovazbyCR <- read.dbf("~/Downloads/SO_VAZBY.dbf") # data o stat obvodech z CSU, cela CR
socodes <- as.character(sovazbyCR$IDSO[sovazbyCR$KOD_CAST_D %in% c("400181","400084")])

urllist <- sapply(socodes, urls_from_SOcodes) %>% flatten()

get_building <- function(url) {
  irsobase <- "http://apl.czso.cz/irso4/"
  df <- read_html(paste0(irsobase, url)) %>%
    html_nodes("table") %>%
    .[3] %>% 
    html_table() %>% as.data.frame()
  df$url <- url
  df
}

get_building_ids <- function(url) {
  irsobase <- "http://apl.czso.cz/irso4/"
  df <- read_html(paste0(irsobase, url)) %>%
    html_nodes("table") %>%
    .[6] %>% 
    html_table(header = NA) %>% as.data.frame()
  names(df) <- c("udaj","kod")
  df$url <- url
  df
}

get_building_basic <- function(url) {
  irsobase <- "http://apl.czso.cz/irso4/"
  df <- read_html(paste0(irsobase, url)) %>%
    html_nodes("table") %>%
    .[1] %>% 
    html_table(header = NA) %>% as.data.frame()
  df <- df[-8,]
  # View(df)
  names(df) <- c("udaj","kod")
  df$url <- url
  df
}

get_address <- function(url) {
  irsobase <- "http://apl.czso.cz/irso4/"
  finurl <- paste0(irsobase,
                   "budadr.jsp?budid=",str_extract(url, "[0-9]{6,7}"),
                   "&idob=",str_extract(url, "[0-9]{10}"))
  # print(finurl)
  df <- read_html(finurl) %>%
    html_node("table[class=\"data\"]") %>% # only catching first table to avoid errors with vedlejsi adresy
    html_table(header = NA)
  df <- bind_rows(df)
  df$idob <- str_extract(url, "[0-9]{10}")
  df$budid <- str_extract(url, "[0-9]{6,7}")
  names(df)[1:3] <- c("udaj","kod","text")
  df$kod <- as.character(df$kod)
  df
}

buildings <- sapply(urllist, get_building, simplify = F) %>%
  bind_rows() %>% 
  mutate(budid = str_extract(url, "[0-9]{6,7}"),
         idob = str_extract(url, "[0-9]{10}")) %>%
  write_csv("buildings.csv")


building_ids <- sapply(urllist, get_building_ids, simplify = F) %>%
  bind_rows() %>% 
  mutate(budid = str_extract(url, "[0-9]{6,7}"),
         idob = str_extract(url, "[0-9]{10}")) %>% 
  write_csv("building_ids.csv")

# building_basic <- sapply(urllist[1:10], get_building_basic, simplify = F) %>%
#   bind_rows() %>% 
#   mutate(budid = str_extract(url, "[0-9]{6,7}"),
#          idob = str_extract(url, "[0-9]{10}")) %>%
#   write_csv("building_basic.csv")

addresses <- sapply(unique(buildings$url), get_address, simplify = F) %>% 
  bind_rows() %>%
  mutate(udaj = str_replace(udaj, ":", "")) %>%
  write_csv("addresses.csv")
              