# look at ecoregion
library(tidyverse)
library(sf)
library(fasterize)
library(stars)

modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
ecoregion_path <- "/home/a/data/background/ecoregions"


# lc_labels <- read_csv("data/usa_landcover_t1_classes.csv") %>%
#   rename(lc = value, lc_name = name)
#d <- st_read("data/events_w_attributes_cus.gpkg") %>%
d<- read_csv("data/event_attributes.csv") %>%
  filter(ignition_year<2017, ignition_year>2000) %>%
  group_by(l1_ecoregion) %>%
  summarise(events = n(),
            burned_area = (sum(total_area_km2))) %>%
  na.omit()
sum(d$burned_area)
sum(d$events)

write_csv(d, "data/fired_ecoregion_summary_2001-2016.csv")

ecoregions <- st_read(file.path(ecoregion_path, "us_eco_l3.shp")) %>%
  mutate(NA_L1CODE = as.numeric(NA_L1CODE),
         l1_ecoregion = str_to_title(NA_L1NAME)) %>%
  st_transform(crs=modis_crs) %>%
  dplyr::select(NA_L1CODE, l1_ecoregion)

mtbs_points <- st_read("/home/a/data/fire/mtbs/points/mtbs_fod_pts_DD.shp") %>%
  st_transform(st_crs(ecoregions)) %>%
  filter(Ig_Date > as.Date("2000-12-31"))

# this takes like ten minutes 
test <- st_intersection(mtbs_points, ecoregions) %>%
  st_set_geometry(NULL) %>%
  group_by(l1_ecoregion) %>%
  summarise(events = n(),
            burned_area_km2 = sum(Acres)*0.00404686)
sum(test$burned_area_km2)
sum(test$events)

write_csv(test, "data/mtbs_ecoregion_summary_2001-2016.csv")

