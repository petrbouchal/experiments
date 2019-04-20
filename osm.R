library(sf)

osm <- st_read("~/Downloads/czech-republic-latest.osm.pbf", layer = "points",
               options = "OGR_INTERLEAVED_READING=YES")
