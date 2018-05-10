

# Reclassification matrix to remove NA values
mtrx = matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)

# Create yearly composites for all tiles
for (j in 1:length(tiles)){
  for(i in 2001:2017) {
    require(magrittr)
    require(raster)

    tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], ".tif")))

    pb <- txtProgressBar(min = 0, max = length(tile_files), style = 3)

    if(!file.exists(paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep=""))){
      fire = raster::stack(tile_files) %>%
        raster::reclassify(., mtrx) %>%
        raster::calc(., max)

      tfilename = paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep="")

      raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
}

