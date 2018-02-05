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

for (j in 1:length(tiles))  {
  require(magrittr)
  require(raster)
  require(gdalUtils)
  require(dplyr)

  # make list of all hdf files for the aoi and time range
  hdfs = list.files(hdf_months, pattern = ".hdf",
                    recursive = TRUE)

  # split the native filename into a more readable format
  filename = strsplit(hdfs, "\\.") %>%
    lapply(`[`, 2:3) %>%
    lapply(paste, collapse = "_") %>%
    unlist
  rm(hdfs)

  # create the final name to be written out
  outname <- paste0(names, "_", filename, ".tif")

  # make list of all hdf files and full path name
  hdfs_full = list.files(hdf_months, pattern = ".hdf",
                         recursive = TRUE, full.names = TRUE)

  pb <- txtProgressBar(min = 0, max = length(hdfs_full), style = 3)

  for (M in 1:length(hdfs_full)) {
    if(!file.exists(paste0(tif_months, "/", outname[M]))) {

      # get the subdatasets from the hdf file
      sds <- gdalUtils::get_subdatasets(hdfs_full[M])

      for (d in 1:length(names)) {

        # unpack the subdatasets based on name stored in object d
        gdalUtils::gdal_translate(sds[d], dst_dataset = paste0(tif_months, "/", outname[M]))
      }
    }
    setTxtProgressBar(pb, M)
  }
}

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


# Create mosaic of burned area for the lower 48 US for each year
for (k in 2001:2017) {
    require(magrittr)
    require(raster)
    if(!file.exists(paste0(yearly_composites,"/USA_", names, "_", k, ".tif"))){
      tile_files = as.vector(Sys.glob(paste0(tif_year, "/", "*", k, "*", ".tif")))
      final <- lapply(tile_files, raster)
      final <- do.call(merge, final) %>%
        raster::crop(usa_ms) %>%
        raster::mask(usa_ms) %>%
        raster::projectRaster(crs = p4string_ea, res = 500) %>%
        raster::crop(as(usa, "Spatial")) %>%
        raster::mask(as(usa, "Spatial"))

      final_name <- paste0(yearly_composites,"/USA_", names, "_", k, ".tif")
      raster::writeRaster(final, final_name, format = "GTiff")
    }
}

# Create mosaic of burned area for the lower 48 US for each year

for (k in 2001:2017) {
  require(magrittr)
  require(raster)
  if(!file.exists(paste0(yearly_composites,"/WUS_", names, "_", k, ".tif"))){
    tile_files = as.vector(Sys.glob(paste0(yearly_composites, "/USA_", "*", k, "*", ".tif")))
    final <- raster(tile_files) %>%
      raster::crop(as(wus, "Spatial")) %>%
      raster::mask(as(wus, "Spatial"))
    
    final_name <- paste0(yearly_composites,"/WUS_", names, "_", k, ".tif")
    raster::writeRaster(final, final_name, format = "GTiff")
  }
}
 