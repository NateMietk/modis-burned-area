#make yearly landcover mosaics
library(tidyverse)
library(gdalUtils)
library(raster)
years<- 2001:2017
local_output <-"/home/a/data/MCD12Q1_tifs"
local_output_mosaics <- "/home/a/data/MCD12Q1_mosaics"
dir.create(local_output)
# getting the tifs

for(y in years){
lc_files <- list.files(file.path(local_output,y), full.names = TRUE)
dir.create(file.path(local_output,y))
  for(f in lc_files){
    tile <- str_extract(f, "h\\d{2}v\\d{2}")
    out_file <- paste0(local_output,"/",y,"/",y,"_",tile,".tif")
    sds <- f %>%
      gdalUtils::get_subdatasets()
    gdalUtils::gdal_translate(sds[1], dst_dataset = out_file)
   print(paste(y, tile))
  }
}
dir.create(local_output_mosaics)

for(y in years){
  ff <- list.files(file.path(local_output, y), full.names = TRUE)
  rr <- list()
  for(fff in ff){
    rr[[fff]] <- raster(fff)
  }
  names(rr) <- NULL
  rr$fun = mean
  rr$na.rm = TRUE
  xx <- do.call(mosaic,rr)
  writeRaster(xx,paste0(local_output_mosaics,"/","usa_lc_mosaic_",y,".tif"))
}
