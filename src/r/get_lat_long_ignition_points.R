# get ignition LL

library(tidyverse)
library(sf)
library(raster)

template_path <- "/home/a/data/MCD12Q1_mosaics/usa_lc_mosaic_2001.tif"
template <- raster(template_path)
ll <- read_csv("data/modis_burn_events_00_19.csv") %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2)) %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  st_transform(4326) %>%
  group_by(id) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]) %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            ignition_date = first(date))


write_csv(ll,"data/ignition_lat_longs.csv")
