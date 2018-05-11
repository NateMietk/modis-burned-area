# Create yearly composites for the lower 48 states

cl <- makeCluster(detectCores())
registerDoParallel(cl)

# Create mosaic of burned area for the lower 48 US for each year
foreach (k = files, .packages = c('foreach', 'tidyverse', 'raster')) %dopar% {
  foreach (i = 2001:2017) %do% {
    
    if(!file.exists(paste0(yearly_composites,"/USA_", names, "_", i, "_", k, ".tif"))){
      tile_files = as.vector(Sys.glob(paste0(tif_year, "/", "*", i, "*", k,  ".tif")))
      final <- lapply(tile_files, raster)
      final <- do.call(merge, final) %>%
        raster::crop(usa_ms) %>%
        raster::mask(usa_ms)
      
      final_name <- paste0(yearly_composites,"/USA_", names, "_", i, "_", k, ".tif")
      raster::writeRaster(final, final_name, format = "GTiff", overwrite=TRUE)
      system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
      
    }
  }
}
stopCluster(cl)