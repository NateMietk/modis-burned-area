# Create yearly composites for the lower 48 states

# Create mosaic of burned area for the lower 48 US for each year
for (i in 2001:2017) {

  if(!file.exists(paste0(yearly_composites,"/USA_", names, "_", i, ".tif"))){
    tile_files = as.vector(Sys.glob(paste0(tif_year, "/", "*", i, ".tif")))

    final <- lapply(tile_files, raster) %>%
      do.call(merge, .)

    library(snow)
    beginCluster(detectCores()/2)

    final <- final %>%
      raster::projectRaster(crs = p4string_ea,
                            res = 500,
                            method = 'ngb') %>%
      raster::crop(as(usa, 'Spatial')) %>%
      raster::mask(as(usa, 'Spatial'))

    endCluster()

    final_name <- paste0(yearly_composites,"/USA_", names, "_", i, ".tif")

    raster::writeRaster(final, final_name, format = "GTiff", overwrite=TRUE)

    system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
    gc()
  }
}
