#setup parallel backend to use many processors
cores <- detectCores()

# Download all .hdf files from ftp site for the Conterminous US
url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

for (j in 1:length(tiles)){
  filenames <- RCurl::getURL(paste0(url, tiles[j],"/"), userpwd = u_p, v=T, 
                             ftp.use.epsv = FALSE)
  
  # write to a temporary file
  cat(filenames, file = 'tmp.txt')
  
  # read the temporary file as a fixed width text file
  dir_listing <- read_fwf('tmp.txt', fwf_empty('tmp.txt'))
  
  # give columns good names
  names(dir_listing) <- c('z1', 'z2', 'z3', 'z4', 'size_in_bytes', 
                          'month_modified', 'date_modified', 'year_modified', 
                          'filename')
  
  # filter out columns we don't care about
  dir_listing <- dplyr::select(dir_listing, -starts_with('z'))
  
  # iterate over the files and download each
  for (i in 1:nrow(dir_listing)) {
    output_file_name <- file.path(hdf_months, dir_listing$filename[i])
    if (!file.exists(output_file_name)) {
      # download the hdf file
      download.file(paste0(url, tiles[j], "/", dir_listing$filename[i]),
                    output_file_name)
      
      # check size of downloaded file
      local_size <- file.info(output_file_name)$size
      
      # check to see if the downloaded file size is identical to the servers file size
      are_bytes_identical <- identical(as.integer(local_size), dir_listing$size_in_bytes[i])
      
      if (!are_bytes_identical) {
        # add warning if the downloaded file is a fragment of the source file
        warning(paste('Mismatch in file size found for', dir_listing$filename[i]))

        # write the name of the fragement to an output dataframe
        res <- res %>%
            mutate(source_file = dir_listing$filename[i])
        
        # delete the fragment file
        unlink(dir_listing$filename[i])
        }
      }
    }
  }



# Convert .hdfs to .tifs, project to albers equal area, and reclassify to just julian date
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach (j = 1:length(tiles)) %dopar% {
  require(magrittr)
  require(raster)
  require(gdalUtils)
  require(dplyr)
  
  hdfs = list.files(hdf_months, pattern = ".hdf",
                    recursive = TRUE)

  filename = strsplit(hdfs, "\\.") %>%
    lapply(`[`, 2:3) %>%
    lapply(paste, collapse = "_") %>%
    unlist
  rm(hdfs)

  outname <- paste0(names, filename, ".tif")

  hdfs_full = list.files(hdf_months, pattern = ".hdf",
                         recursive = TRUE, full.names = TRUE)

  for (M in 1:length(hdfs_full)) {
    sds <- gdalUtils::get_subdatasets(hdfs_full[M])
    if(!file.exists(paste0(tif_months, "/", outname[M]))) {
      gdalUtils::gdal_translate(sds[1], dst_dataset = paste0(tif_months, "/", outname[M]))
    }
  }
}
stopCluster(cl)

# Create yearly composites for all tiles
cl <- makeCluster(cores/2)
registerDoParallel(cl)
foreach (j = 1:length(tiles)) %dopar% {
  for(i in 2001:2017) {
    require(magrittr)
    require(raster)
    tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", i, "*", tiles[j], ".tif")))
    
    # Reclassification matrix to remove NA values
    mtrx = matrix(c(-Inf, 1, 0, 366, Inf, 0), byrow=TRUE, ncol=3)
    
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

      final_name <- paste0(final_output,"/WUS_", names, "_", k, ".tif")
      raster::writeRaster(final, final_name, format = "GTiff")
    }
  }
