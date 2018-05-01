# script for calculating reburns within years

#reclassifying to binary
mtrx = matrix(c(-Inf, 0.5, 0, 368, Inf, 0, .5,368,1), byrow=TRUE, ncol=3)

years = 2001:2017
# Create yearly composites for all tiles

for (j in 1:length(tiles)){
  for(i in years) {
    require(magrittr)
    require(raster)
    
    tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], ".tif")))
    
    pb <- txtProgressBar(min = 0, max = length(tile_files), style = 3)
    
    if(!file.exists(paste(tif_year, "/Yearly_n_", tiles[j], "_", i, ".tif", sep=""))){
      fire = raster::stack(tile_files) %>%
        raster::reclassify(., mtrx) %>%
        raster::calc(., sum)
      
      tfilename = paste(tif_year, "/Yearly_n_", tiles[j], "_", i, ".tif", sep="")
      
      raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
}

# then tally the numbers
rasts <- list.files(tif_year)

for(i in 1:length(rasts)){
  x <- raster(rasts[i])
  y <- getValues(x)
  table(y)
}
