library(sf)
library(tibble)

df0 <- tribble(~id, ~lon, ~lat,
               "a",    1,    1,
               "b",    2,    1)
df1 <- tribble(~id, ~lon, ~lat,
               "a",    1,    1,
               "b",   NA,    1)

df0_geo <- st_as_sf(df0, coords = c("lon", "lat"), crs = 5514)
st_transform(df0_geo, 4326)

df1_geo <- st_as_sf(df1, coords = c("lon", "lat"), crs = 5514)
st_transform(df1_geo, 4326) # hangs

sessionInfo()
