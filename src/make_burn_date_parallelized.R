
# Download all .hdf files from ftp site for the Conterminous US
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach(j = 1:length(tiles)) %dopar% {
  filenames <- RCurl::getURL(paste0(url,tiles[j],"/"), userpwd = u_p, v=T, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames = paste0(strsplit(filenames, "\r*\n")[[1]])
  for(L in 1:length(filenames)){
    output_file_name <- file.path(hdf_months, filenames[L])
    if(!file.exists(output_file_name)) {
      download.file(paste0(url,tiles[j],"/",filenames[L]),
                    output_file_name)
    }
  }
}
stopCluster(cl)

# Convert .hdfs to .tifs, project to albers equal area, and reclassify to just julian date
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach (j = 1:length(tiles)) %dopar% {
  require(magrittr)
  require(raster)
  require(MODIS)

  hdfs = list.files(hdf_months, pattern = ".hdf",
                    recursive = TRUE)

  filename = strsplit(hdfs, "\\.") %>%
    lapply(`[`, 2:3) %>%
    lapply(paste, collapse = "_") %>%
    unlist

  outname <- paste0(names, filename, ".tif")

  hdfs_full = list.files(hdf_months, pattern = ".hdf",
                         recursive = TRUE, full.names = TRUE)

  for (M in 1:length(hdfs_full)) {
    sds <- MODIS::getSds(hdfs_full[M])
    r <- raster::raster(sds$SDS4gdal[d])
    if(!file.exists(paste0(tif_months, "/", outname[M]))) {
      raster::writeRaster(r, paste0(tif_months, "/", outname[M]), overwrite=TRUE)
    }
  }
}
stopCluster(cl)

# Create yearly composites for all tiles
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach (j = 1:length(tiles)) %dopar% {
  for(i in 2001:2017) {
    require(magrittr)
    require(raster)
    tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], ".tif")))

    if(!file.exists(paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep=""))){
      fire = raster::stack(tile_files) %>%
        raster::reclassify(., mtrx) %>%
        raster::calc(., max)

      tfilename = paste(tif_year, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep="")

      raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
    }
  }
}
stopCluster(cl)


# Create mosaic of burned area for the lower 48 US for each year
for(k in 2001:2017) {
    require(magrittr)
    require(raster)
    if(!file.exists(paste0(final_output,"/USA_", names, "_", k, ".tif"))){
      tile_files = as.vector(Sys.glob(paste0(tif_year, "/", "*", k, "*", ".tif")))
      final <- lapply(tile_files, raster)
      final <- do.call(merge, final) %>%
        raster::crop(wus_ms) %>%
        raster::mask(wus_ms) %>%
        raster::projectRaster(crs = p4string_ea, res = 500) %>%
        raster::crop(as(wus_shp, "Spatial")) %>%
        raster::mask(as(wus_shp, "Spatial"))

      final_name <- paste0(final_output,"/USA_", names, "_", k, ".tif")
      raster::writeRaster(final, final_name, format = "GTiff")
    }
  }
