# combining eastern and western hemispheres
library(tidyverse)
library(sf)
library(units)
system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/noedge data/finished_polys")


wh_noedge <- list.files("data/finished_polys", pattern = "gpkg",full.names = TRUE)[1:64]
eh_noedge <- list.files("data/finished_polys", pattern = "gpkg",full.names = TRUE)[65:195]

wh_l <- list()
for(i in 1:length(wh_noedge)){
  wh_l[[i]] <- st_read(wh_noedge[i])
}

wh_stitched_edges <- st_read("wh_edges_stitched.gpkg") %>%
  dplyr::rename(last_date = end_date) %>%
  mutate(area_burned_ha = drop_units(st_area(.)*0.0001))

wh_all <- do.call("rbind", wh_l) %>%
  rbind(wh_stitched_edges)


st_write(wh_all, "western_hemisphere_to_may2019.gpkg")
