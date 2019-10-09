#rbind eh

library(tidyverse)
# install.packages("snowfall")
# library(snowfall)
library(sf)

system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/noedge data/noedge")

ne_files <- list.files("data/noedge", pattern = "gpkg", full.names=T)[1:64]

e_files <- list.files(pattern="wh_edges_stitched.gpkg", full.names=T)

e_ne<-c(e_files, ne_files)

file_list<-list()

for(i in 1:length(e_ne)){
  file_list[[i]]<- st_read(e_ne[i])
}

wh<- do.call("rbind", file_list) # takes about an hour
st_write(wh, "western_hemisphere_tomay2019.gpkg", delete_dsn = TRUE)
system("aws s3 cp western_hemisphere_tomay2019.gpkg s3://earthlab-natem/modis-burned-area/delineated_events/western_hemisphere_to_may2019.gpkg")
