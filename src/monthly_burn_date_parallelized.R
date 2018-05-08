#setup parallel backend to use many processors
cores <- detectCores()-1
monthly <- "data/MCD64A1/C6/monthly_composites"
s3_path <- "s3://earthlab-natem/data/disturbances/fire/mcd64a1/monthly_composites"

all_files <- list.files(tif_months)
month_years <- unique(substr(all_files, 11,17))

# Reclassification matrix to remove NA values

registerDoParallel(cores)
# Create mosaic of burned area for the lower 48 US for each year
foreach (k = month_years) %dopar% {
    require(magrittr)
    require(raster)
  mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
    outfile <- paste0(monthly,"/USA_", names, "_", k, ".tif")
    if(!file.exists(outfile)){
      tile_files <- as.vector(Sys.glob(paste0(tif_months, "/", "*", k, "*", ".tif")))
      final <- lapply(tile_files, raster)
      final <- do.call(merge, final) %>%
        raster::reclassify(mtrx) %>%
        raster::crop(usa_ms) %>%
        raster::mask(usa_ms) # %>%
        # raster::projectRaster(crs = p4string_ea, res = 500) %>%
        # raster::crop(as(usa, "Spatial")) %>%
        # raster::mask(as(usa, "Spatial"))
      final_name <- paste0(monthly,"/USA_", names, "_", k, ".tif")
      raster::writeRaster(final, final_name, format = "GTiff")
      system(paste("aws s3 cp",
                   final_name,
                   paste0(s3_path, "/USA_", names, "_", k, ".tif")))
    }
    gc()
}
