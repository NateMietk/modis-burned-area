
# -----------------------------------------------------

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