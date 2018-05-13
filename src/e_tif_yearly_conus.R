# Create yearly composites for the lower 48 states

cl <- makeCluster(1)
registerDoParallel(cl)

# Create mosaic of burned area for the lower 48 US for each year
foreach (i = 2001:2017, .packages = c('foreach', 'tidyverse', 'raster')) %dopar% {

  if(!file.exists(paste0(yearly_composites,"/USA_", names, "_", i, ".tif"))){
    tile_files = as.vector(Sys.glob(paste0(tif_year, "/", "*", i, ".tif")))

    final <- lapply(tile_files, raster) %>%
      do.call(merge, .) %>%
      raster::crop(usa_ms) %>%
      raster::mask(usa_ms) %>%
      raster::projectRaster(., crs = p4string_ea, res = 500, method = 'ngb') %>%
      raster::crop(usa) %>%
      raster::mask(usa)

    final_name <- paste0(yearly_composites,"/USA_", names, "_", i, ".tif")

    raster::writeRaster(final, final_name, format = "GTiff", overwrite=TRUE)

    system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
  }
}

stopCluster(cl)
