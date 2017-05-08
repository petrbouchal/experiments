library(tidyverse)
library(readxl)
library(readr)
library(foreign)
library(sf)
library(leaflet)
library(leaflet.extras)
library(stringr)

# priklad budovy http://apl.czso.cz/irso4/buddet.jsp?x=1&idob=1000153843&budid=685096

proper_krovak <- "+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=30.28813972222222 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +pm=greenwich +units=m +no_defs +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56"
# odsud: http://freegis.fsv.cvut.cz/gwiki/S-JTSK
# protože CSR JTSK je v PROJ4 špatně, chyba je v Praze cca 9 metrů

buildings_pocobyv <- read_csv("buildings.csv") %>%
  filter(Údaj == "Počet evidovaných obyvatel v budově:") %>% 
  spread(Údaj, Kód) %>% 
  rename(pocobyv_evid = `Počet evidovaných obyvatel v budově:`) %>% 
  select(-Hodnota)

building_ids_iskn <- read_csv("building_ids.csv") %>% 
  filter(udaj == "V Informačním systému katastru nemovitostí:") %>% 
  select(-udaj) %>% 
  rename(budid_katastr = kod)

# buildings_basic <- read_csv("building_basic.csv")

# building_basic_ulice <- building_basic %>% 
#   filter(udaj == "Ulice:") %>% 
#   select(-udaj) %>% 
#   rename(ulice = kod)

addresses_id_ruian <- read_csv("addresses.csv") %>%
  mutate(udaj = str_replace(udaj, "Identifikátor adresy RÚIAN", "id_adr_ruian")) %>% 
  filter(udaj == "id_adr_ruian") %>%
  rename(id_adr_ruian = kod) %>%
  mutate(id_adr_ruian = as.numeric(id_adr_ruian)) %>% 
  select(-udaj, -text)

adresy_praha <- read_csv2("~/Downloads/20170331_OB_554782_ADR.csv",
                          locale = locale(encoding = "WINDOWS-1250", decimal_mark = ".")) %>% 
  filter(`Kód části obce` == 490156)

adresy_vazby_cr <- read_csv2("~/Downloads/strukturovane-CSV/adresni-mista-vazby-cr.csv",
                             locale = locale(encoding = "WINDOWS-1250", decimal_mark = ".")) %>% 
  filter(COBCE_KOD == 490156)

sonas <- buildings_pocobyv %>% 
  right_join(addresses_id_ruian) %>%
  mutate(id_adr_ruian = as.integer(id_adr_ruian)) %>% 
  left_join(adresy_praha %>%  rename(id_adr_ruian = `Kód ADM`)) %>% 
  rename(sour_x = `Souřadnice X`,
         sour_y = `Souřadnice Y`) %>% 
  mutate(sour_x = -sour_x/100,
         sour_y = -sour_y/100) %>% 
  filter(!is.na(sour_x) & !is.na(sour_y)) %>%
  st_as_sf(coords = c("sour_y","sour_x"), crs = proper_krovak) %>% 
  st_transform(sonas, crs = 4326)

class(sonas)
st_bbox(sonas)
st_crs(sonas)
st_geometry(sonas)
plot(sonas, max.plot = 2)
ggplot(sonas) + geom_sf()

ciselnik_pocobyv <- read_csv("./geodata/ciselnik_pocobyv.csv")

sonas_kat <- buildings_pocobyv %>%
  inner_join(building_ids_iskn) %>% 
  left_join(ciselnik_pocobyv) %>% 
  rename(ID_2 = budid_katastr) %>%
  mutate(ID_2 = as.character(ID_2)) %>% 
  replace_na(replace = list("pocobyv_evid"=0)) %>% 
  group_by(ID_2, Text, url) %>% # means idob and budid have to be dropped - cannot link other data then
  summarise(pocobyv_evid = sum(pocobyv_evid, na.rm = T), mean = sum(mean, na.rm = T)) %>% 
  ungroup()

# Katastr shapefile budov a parcel http://services.cuzk.cz/shp/ku/epsg-5514/ then Nusle
# vse o katastru http://geoportal.cuzk.cz/(S(k42xogxem011exslchr52y4m))/Default.aspx?mode=TextMeta&metadataXSL=full&side=WFS.ATOM_KM&metadataID=CZ-00025712-CUZK_ATOM-MD_KM-KU-SHP

ktstr_nsle_bud <- st_read("~/Downloads/728161/BUDOVY_P.shp", stringsAsFactors = F,
                          crs = proper_krovak) %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = T)

ktstr_vnhrd_bud <- st_read("~/Downloads/727164//BUDOVY_P.shp", stringsAsFactors = F,
                          crs = proper_krovak) %>% 
  st_transform(crs = 4326) %>% 
  st_zm(drop = T)

ktstr_bud <- rbind(ktstr_nsle_bud, ktstr_vnhrd_bud)

class(ktstr_nsle_bud)

sonas_kat2 <- ktstr_bud %>%
  inner_join(sonas_kat) %>% 
  left_join(addresses_id_ruian) %>% 
  inner_join(adresy_praha %>% rename(id_adr_ruian = `Kód ADM`)) %>% 
  filter(!is.na(st_dimension(geometry))) %>% 
  mutate(adresa_display = paste0(`Název ulice`, " ",
                                 `Číslo domovní`,"/",`Číslo orientační`))

class(sonas_kat2)
st_bbox(sonas_kat2)
st_crs(sonas_kat2)
st_geometry(sonas_kat2)

pal <- colorNumeric(
  palette = "Reds",
  domain = sonas_kat2$mean)

leaflet() %>%
  # setView(lng = 14.4405, lat = 50.08, zoom = 13) %>%
  # addTiles(options = tileOptions(detectRetina = T)) %>%
  addProviderTiles(provider = "Stamen.TonerLite",
                   options = tileOptions(detectRetina = T)) %>%
  # addCircleMarkers(data = vop4,
  #            clusterOptions = markerClusterOptions(),
  #            popup = ~as.character(adresa)) %>% 
  # addPolygons(data = mcP, stroke = T, popup = ~NAZEV_1, fill = F, weight = 2) %>% 
  # addCircleMarkers(radius = ~pocobyv_evid, color = ~pal(pocobyv_evid),
  #                  stroke = F, fillOpacity = 1,
  #                  fillColor = ~pal(pocobyv_evid), fill = T) %>% 
  addPolygons(data = st_geometry(sonas_kat2), fill = T,
              fillColor = pal(sonas_kat2$mean),
              stroke = F, fillOpacity = 1, popup = sonas_kat2$adresa_display) %>%
  # addPolygons(data = mcP, stroke = T, popup = ~NAZEV_1, fill = F, weight = 2) %>% 
  # addPolylines(data = st_geometry(uliceNusle_lines)) %>% 
  # addPolygons(data = st_geometry(ktstr_nsle_bud)) %>% 
  # addCircleMarkers(data = st_geometry(sonas))
  # addHeatmap(blur = 20, max = 0.05,
  #            intensity = ~pocobyv_evid,
  #            radius = 15) %>%
  suspendScroll()

# http://leaflet-extras.github.io/leaflet-providers/preview/

uliceNusle_kody <- as.numeric(as.character(uliceP$KOD[uliceP$COBCEK==400181]))
uliceNusle_lines <- uliceP_lines[uliceP_lines$Kod %in% uliceNusle_kody,]

ggplot(sonas_kat2) +
  geom_sf(aes(fill = mean), color = NA) +
  geom_sf(data = uliceNusle_lines)

plot(uliceNusle_lines, max.plot = 1)

leaflet() %>% 
  addPolygons(data = st_geometry(ktstr_nsle_bud))
