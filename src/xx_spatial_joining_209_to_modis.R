# spatial join 209 data to modis
library(tidyverse)
library(raster)
library(sf)

events_table_filename <- "data/ics209_allWFincidents1999to2014.csv"
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 
mtbs_filename <- "/home/a/data/fire/mtbs/mtbs_fod_pts_20170501.shp"

events_209 <- read_csv(events_table_filename) 

s3_path<- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_events"

years <- 2001:2014
results_df <- data.frame(year = NA, total_events = NA, matches = NA, percent_match = NA)
for(i in 1:length(years)){
  events_raster_filename <- file.path(paste0("USA_BurnDate_",years[i],"_events.tif"))
  system(paste("aws s3 cp", 
               file.path(s3_path, events_raster_filename),
               file.path("data", events_raster_filename)
         ))
  
  events_modis_r <- raster(file.path("data",events_raster_filename))
  
  results_df[i,1] <- years[i]
  
  events_209 <- read_csv(events_table_filename)%>%
    filter(is.na(POO_LATITUDE)==FALSE,
           START_YEAR==years[i])
  
  results_df[i,2] <- nrow(events_209)
  
  events_209 <- events_209  %>%
    st_as_sf(coords=c("POO_LONGITUDE","POO_LATITUDE"), crs = latlong) %>%
    st_transform(crs=crs(events_modis_r, asText=T)) %>%
    st_buffer(1000) %>%
    mutate(modis_id = raster::extract(events_modis_r, ., buffer = 2000, fun = max)) %>%
    filter(modis_id>0) %>%
    filter(modis_id = paste("event", years[i], modis_id, sep="_"))
  
  results_df[i,3] <- nrow(events_209)
  results_df[i,4] <- results_df[i,3]/results_df[i,2]
  
}
