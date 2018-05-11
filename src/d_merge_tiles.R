
files <- c('raw', 'cleaned', 'filled')

# Create yearly composites for all tiles
for (k in files) {
  for (j in 1:length(tiles)){
    for(i in 2001:2017) {
      require(magrittr)
      require(raster)
      
      tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], '*', k, ".tif")))
      
      pb <- txtProgressBar(min = 0, max = length(tile_files), style = 3)
      
      if(!file.exists(paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, "_", k, ".tif", sep=""))){
        fire = raster::stack(tile_files) %>%
          raster::calc(., max)
        
        tfilename = paste(tif_year, "/Yearly_BD_", tiles[j], "_", i,  "_", k, ".tif", sep="")
        
        raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
      }
      setTxtProgressBar(pb, i)
      system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
    }
  }
}


# Create mosaic of burned area for the lower 48 US for each year
for (k in files) {
  for (i in 2001:2017) {
    require(magrittr)
    require(raster)
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