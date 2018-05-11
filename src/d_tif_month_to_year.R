
# Create yearly composites for all tiles

cl <- makeCluster(detectCores())
registerDoParallel(cl)

for (k in files) {
  foreach (j = 1:length(tiles), .packages = c('foreach', 'tidyverse', 'raster')) %dopar% {
    foreach (i = 2001:2017) %do% {
      
      tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], '*', k, ".tif")))
      
      #pb <- txtProgressBar(min = 0, max = length(tile_files), style = 3)
      
      if(!file.exists(paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, "_", k, ".tif", sep=""))){
        fire = raster::stack(tile_files) %>%
          raster::calc(., max)
        
        tfilename = paste(tif_year, "/Yearly_BD_", tiles[j], "_", i,  "_", k, ".tif", sep="")
        
        raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
      }
      #setTxtProgressBar(pb, i)
      system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
    }
  }
}

stopCluster
