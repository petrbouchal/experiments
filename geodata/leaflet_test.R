library(geojsonio)
library(leaflet)
library(rgeos)
library(rgdal)
library(ggplot2)

# https://rud.is/b/2014/09/26/overcoming-d3-cartographic-envy-with-r-ggplot/
  

generel <- geojson_read("http://opendata.iprpraha.cz/CUR/DOP/DOP_Cyklogenerel_l/WGS_84/DOP_Cyklogenerel_l.json")
cstezky <- geojson_read("http://opendata.iprpraha.cz/CUR/DOP/DOP_Cyklotrasy_l/WGS_84/DOP_Cyklotrasy_l.json")
odpady <- geojson_read("http://opendata.iprpraha.cz/CUR/ZPK/ZPK_O_SberOdpadu_b/S_JTSK/ZPK_O_SberOdpadu_b.json")

jsonogred <- readOGR("http://opendata.iprpraha.cz/CUR/DOP/DOP_Cyklogenerel_l/WGS_84/DOP_Cyklogenerel_l.json")
jsonogredodp <- readOGR("http://opendata.iprpraha.cz/CUR/ZPK/ZPK_O_SberOdpadu_b/S_JTSK/ZPK_O_SberOdpadu_b.json")
str(jsonogred)
str(jsonogred@data)

# convert coordinates in odpady file

odpady$features[[1]]$geometry$coordinates

krovak2wgs84 <- function(dataset) {
  library(broom)
  library(rgeos)
  library(rgdal)
  souradnice <- as.data.frame(dataset@coords)
  names(souradnice) <- c("lat","lon")
  coordinates(souradnice) <- c("lat","lon")
  proj4string(souradnice) <- CRS("+proj=krovak +lat_0=49.5 +lon_0=24.83333333333333 +alpha=0 +k=0.9999 +x_0=0 +y_0=0 +ellps=bessel +towgs84=570.8,85.7,462.8,4.998,1.587,5.261,3.56 +units=m +no_defs")
  proj4string(souradnice) <- CRS("++init=epsg:5514")
  crs_new <- CRS("+init=epsg:4326")
  
  sour2 <- spTransform(souradnice, crs_new)
  
  dataset@coords <- sour2@coords
  dataset@bbox <- sour2@bbox
  proj4string(dataset) <- CRS("++init=epsg:4326")
  
  geojson_write(dataset, file = "newodpady")
  dataset_new <- readOGR("newodpady.geojson","OGRGeoJSON")
  
  odpady_df <- broom::tidy(dataset_new)
  odpady_df
}

odpady_wgs84 <- krovak2wgs84(jsonogredodp)

generelogred <- readOGR("http://opendata.iprpraha.cz/CUR/DOP/DOP_Cyklogenerel_l/WGS_84/DOP_Cyklogenerel_l.json", "OGRGeoJSON")
generel_df <- broom::tidy(generelogred)

# map

leaflet(data = odpady_wgs84) %>% setView(lng = 14.45, lat = 50.1, zoom = 11) %>%
  addTiles(options = tileOptions(detectRetina = T)) %>%
  addCircleMarkers(lng = ~coords.x1, lat = ~coords.x2)

plot(sour2)

leaflet() %>% setView(lng = 14.45, lat = 50.1, zoom = 11) %>%
  addTiles(options = tileOptions(detectRetina = T)) %>%
  # addGeoJSON(cstezky, fill = F, color = "red") %>% 
  addGeoJSON(generel, fill = F) %>%
  addPolylines(data = generelogred@lines, popup = generelogred@data$NAZEV)
# addMarkers(data = odpadynewogred@coords, popup = odpadynewogred@data$NAZEV)

sfread <- sf::st_read("http://opendata.iprpraha.cz/CUR/DTMP/TMMESTSKECASTI_P/S_JTSK/TMMESTSKECASTI_P.json")
plot(sfread)
sfread_reproj <- sf::st_transform(x = sfread, crs = "++init=epsg:4326")

ggplot() + geom_sf(data = sfread_reproj)

table(jsonogred@data$KATEGORIE)
