Sys.setlocale(category = "LC_ALL", locale = "cs_CZ.UTF-8")
library(shapefiles)
library(sp)
library(spatstat)
library(mapproj)
library(maptools)
library(rgdal)
library("ggplot2")
library("ggmap")
library(dplyr)
library(rgeos)
library(extrafont)
library(grid)
library(scales)
library(readr)

# need to manually open file using Windows Latin 2 and save as UTF-8

obyv <- read.csv('~/Documents/Research/Scitani2011/SLDB_OBYVATELSTVO_UTF8.csv')
uzemi <-  read.csv('~/Documents/Research/Scitani2011/seznam_uzemi_UTF8.csv')

obyv <- obyv %>%
  select(typuz_naz, nazev, uzcis, uzkod, vse5131, vse1111) %>% 
  mutate(stat = vse5131 / vse1111) %>% 
  filter(typuz_naz == 'obec') %>% 
  merge(uzemi, by = c('nazev','typuz_naz'),all.x = F, all.y = F)

unique(obyv$kodnuts)

# Load map shape data
setwd('~/Documents/Research/Geodata/CUZK//')
mapdata <- readOGR(dsn = '.',layer = 'SPH_OBEC', encoding = 'LATIN2')
mapdata <- thinnedSpatialPoly(mapdata, 0.005, topologyPreserve = TRUE)

coordinates <-
  data.frame(long = coordinates(mapdata)[, 1],lat = coordinates(mapdata)[, 2])
coordinates[, 'NAZEV_LAU2'] <-
  as.character(mapdata@data[,'NAZEV_LAU2'])
coordinates[, 'KOD_LAU2'] <- as.character(mapdata@data[,'KOD_LAU2'])

mapdataset <- fortify(mapdata, region = 'KOD_LAU2')
mapdataset$id <- as.factor(mapdataset$id)

# Create GeoJSON
writeOGR(mapdata, 'GeoJSON', layer = "", driver = "GeoJSON")

ggplot(mapdataset, aes(x=long, y=lat)) +
  geom_polygon(aes(group=group), fill='red', colour='blue') +
  coord_map()

plotdata <-
  merge(mapdataset, obyv, by.x = 'id',by.y = 'kodnuts',all.x = TRUE)
plotdata <- arrange(plotdata, order)

#

# pdf()
ggplot(plotdata, aes(x = long, y = lat)) +
  geom_polygon(aes(
    group = group, fill = stat, colour = stat
  )) +
  coord_map() +
  geom_text(aes(x = long, y = lat, label = NAZEV_LAU2), data = coordinates) +
  theme(text = element_text(family = 'Fira Sans', size = 3))
# dev.off()

plotdata$rowid <- 1:nrow(plotdata)
all_values <- function(x) {
  if (is.null(x))
    return(NULL)
  row <- plotdata[plotdata$rowid == x$rowid,]
  paste0(
    "<div style=\'font-family:sans-serif;\'>Kraj: ",
    as.character(row$nazev.x), "<br />Statistika: ", row$stat,'</div>'
  )
}

plot(coordinates)

library(ggvis)
plotdata %>% ggvis( ~ long, ~ lat, key :=  ~ rowid) %>%
  group_by(group,id) %>%
  layer_paths(stroke =  ~ stat, fill =  ~ stat, strokeOpacity = 0) %>%
  set_options(width = 600, height = 400, keep_aspect = TRUE) %>%
  hide_legend(c("fill","stroke")) %>%
  add_tooltip(all_values)

library("rCharts")

jsonmap = jsonlite::fromJSON('GeoJSON')

# rmap <- Leaflet$new()
# rmap$setView(c(49, 19), zoom = 5)
# rmap$tileLayer(provider = 'Stamen.Watercolor')
# rmap$geoJson(jsonmap,
#              style = "#! function(feature) {
#     var rgn2col = {0:'red',1:'blue',2:'green'};
#     return {
#       color: rgn2col[feature.properties['id']],
#       strokeWidth: '1px',
#       strokeOpacity: 1,
#       fillOpacity: 1
#     }; } !#")
# rmap

# HTMLWidgets and leaflet

library(htmlwidgets)
library(leaflet)

plotdata <- group_by(plotdata, id)

ll <- leaflet(plotdata) %>%
  addTiles() %>%
  addMultiPolygons(lat =  ~ lat, lng =  ~ long)
ll

# LeafletR
library(leafletR)
q.dat <-
  toGeoJSON(mapdataset, dest = tempdir(), lat.lon = c("lat", 'long'))
leafletR::leaflet(q.dat)

# GGMap
library(ggmap)
ggmap(get_map(
  location = c(15.5,50), zoom = 7, source = 'stamen', maptype = 'toner-lite', color = 'bw'
)) +
  geom_polygon(
    aes(
      x = long, y = lat, group = group, fill = stat
    ), colour = 'lightgrey',
    size = .2, data = plotdata, alpha = .7
  )
ggsave('aa.pdf')
