
# -----------------------------------------------------
source("src/a_prep_environment.R")

system("aws s3 sync s3://earthlab-natem/data/disturbances/fire/mcd64a1/tif_months data/MCD64A1/C6/tif_months")

cl <- makeCluster(detectCores())
registerDoParallel(cl)

foreach (j = 1:length(tiles), .packages = c('gdalUtils', 'tidyverse', 'raster')) %dopar% {

  raw_dates <- function(input_raster, output = tif_months) {

    month_range <- strsplit(input_raster, "\\_") %>%
      lapply(`[`, 2) %>%
      substr(6, 8)

    mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)

    ras <- raster(file.path(output, input_raster)) %>%
      raster::reclassify(mtrx)

    writeRaster(ras, file.path(output, input_raster), format = "GTiff", overwrite=TRUE)

  }

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

  for (M in 1:length(hdfs_full)) {
    if(!file.exists(paste0(tif_months, "/", outname[M]))) {

      # get the subdatasets from the hdf file
      sds <- gdalUtils::get_subdatasets(hdfs_full[M])

      for (d in 1:length(names)) {

        # unpack the subdatasets based on name stored in object d
        gdalUtils::gdal_translate(sds[d], dst_dataset = paste0(tif_months, "/", outname[M]))

        raw_dates(outname[M])
      }
    }
    system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
  }
}

stopCluster(cl)
