#make yearly landcover mosaics
library(tidyverse)
library(gdalUtils)
library(raster)
years<- 2001:2017
dir.create("/home/a/data/MCD12Q1_tifs/")
# getting the tifs

for(y in years){
lc_files <- list.files(file.path("/home/a/data/MCD12Q1",y), full.names = TRUE)
dir.create(paste0("/home/a/data/MCD12Q1_tifs/",y))
  for(f in lc_files){
    tile <- str_extract(f, "h\\d{2}v\\d{2}")
    out_file <- paste0("/home/a/data/MCD12Q1_tifs/",y,"/",y,"_",tile,".tif")
    sds <- f %>%
      gdalUtils::get_subdatasets()
    gdalUtils::gdal_translate(sds[1], dst_dataset = out_file)
   print(paste(y, tile))
  }
}
dir.create("/home/a/data/MCD12Q1_mosaics")

for(y in years){
  ff <- list.files(file.path("/home/a/data/MCD12Q1_tifs", y), full.names = TRUE)
  rr <- list()
  for(fff in ff){
    rr[[fff]] <- raster(fff)
  }
  names(rr) <- NULL
  rr$fun = mean
  rr$na.rm = TRUE
  xx <- do.call(mosaic,rr)
  writeRaster(xx,paste0("/home/a/data/MCD12Q1_mosaics/","usa_lc_mosaic_",y,".tif"))
}
