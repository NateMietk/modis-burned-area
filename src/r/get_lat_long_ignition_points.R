# get ignition LL

library(tidyverse)
library(sf)
library(raster)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

s3_path <- "s3://earthlab-natem/modis-burned-area/delineated_events"
cus_path <- "/home/a/data/background/CUS/"
template_path <- "/home/a/data/MCD12Q1_mosaics/usa_lc_mosaic_2001.tif"
raw_events_file <- "data/modis_burn_events_00_19.csv"


template <- raster(template_path)

cus <- st_read(cus_path) %>%
  st_transform(4326) %>%
  dplyr::select(state = NAME) 

ll <- read_csv(raw_events_file) %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2)) %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  st_transform(4326) %>%
  group_by(id) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  st_intersection(cus) %>%
  mutate(latitude = st_coordinates(.)[,2],
         longitude = st_coordinates(.)[,1]) %>%
  st_set_geometry(NULL) %>%
  group_by(id) %>%
  summarise(latitude = mean(latitude),
            longitude = mean(longitude),
            ignition_date = first(date),
            ignition_state = getmode(state))


write_csv(ll,"data/ignition_lat_longs.csv")
system(paste("aws s3 cp data/ignition_lat_longs.csv",
             file.path(s3_path,"ignition_lat_longs.csv")))
