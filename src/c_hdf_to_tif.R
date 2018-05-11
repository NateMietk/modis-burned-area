
# -----------------------------------------------------
source("src/a_prep_environment.R")

system("aws s3 sync s3://earthlab-natem/data/disturbances/fire/mcd64a1/tif_months data/MCD64A1/C6/tif_months")

raw_dates <- function(input_raster, output = tif_months) {
  
  month_range <- strsplit(input_raster, "\\_") %>%
    lapply(`[`, 2) %>%
    substr(6, 8)
  
  mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
  
  ras <- raster(file.path(output, input_raster)) %>%
      raster::reclassify(mtrx)
    
  writeRaster(ras, file.path(output, input_raster), format = "GTiff", overwrite=TRUE)
  
}

clean_dates <- function(input_raster, output = tif_months) {
  
  outname <- gsub('_raw.tif', '_cleaned.tif', input_raster)
  month_range <- strsplit(input_raster, "\\_") %>%
    lapply(`[`, 2) %>%
    substr(6, 8)
  
  ras <- raster(file.path(output, input_raster))
  
  reclass_jan <-
    matrix(c(-Inf, 1, NA, 032, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_feb <-
    matrix(c(-Inf, 1, NA, 1, 032, NA, 060, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_march <-
    matrix(c(-Inf, 1, NA, 1, 060, NA, 091, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_april <-
    matrix(c(-Inf, 1, NA, 1, 091, NA, 121, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_may <-
    matrix(c(-Inf, 1, NA,  1, 121, NA, 152, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_june <-
    matrix(c(-Inf, 1, NA, 1, 152, NA, 182, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_july <-
    matrix(c(-Inf, 1, NA, 1, 182, NA, 213, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_august <-
    matrix(c(-Inf, 1, NA, 1, 213, NA, 244, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_september <-
    matrix(c(-Inf, 1, NA, 1, 244, NA, 274, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_october <-
    matrix(c(-Inf, 1, NA, 1, 274, NA, 305, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_november <-
    matrix(c(-Inf, 1, NA, 1, 305, NA, 335, 366, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  reclass_december <-
    matrix(c(-Inf, 1, NA, 1, 335, NA, 367, Inf, NA),
           byrow = TRUE,
           ncol = 3)
  
  
  if(month_range == '001') {
    ras <- ras %>%
      raster::reclassify(reclass_jan)
    
  } else if(month_range == '032') {
    ras <- ras %>%
      raster::reclassify(reclass_feb)
    
  } else if(month_range == '060') {
    ras <- ras %>%
      raster::reclassify(reclass_march)
    
  } else if(month_range == '091') {
    ras <- ras %>%
      raster::reclassify(reclass_april)
    
  } else if(month_range == '121') {
    ras <- ras %>%
      raster::reclassify(reclass_may)
    
  } else if(month_range == '152') {
    ras <- ras %>%
      raster::reclassify(reclass_june)
    
  } else if(month_range == '182') {
    ras <- ras %>%
      raster::reclassify(reclass_july)
    
  } else if(month_range == '213') {
    ras <- ras %>%
      raster::reclassify(reclass_august)
    
  } else if(month_range == '244') {
    ras <- ras %>%
      raster::reclassify(reclass_september)
    
  }  else if(month_range == '274') {
    ras <- ras %>%
      raster::reclassify(reclass_october)
    
  } else if(month_range == '305') {
    ras <- ras %>%
      raster::reclassify(reclass_november)
    
  } else if(month_range == '335') {
    ras <- ras %>%
      raster::reclassify(reclass_december)
  }
  
  
  writeRaster(ras, file.path(output, outname), format = "GTiff", overwrite=TRUE)
  
}

fill_dates <- function(input_raster, output = tif_months) {
  
  outname <- gsub('_raw.tif', '_filled.tif', input_raster)
  month_range <- strsplit(input_raster, "\\_") %>%
    lapply(`[`, 2) %>%
    substr(6, 8)
  
  ras <- raster(file.path(output, input_raster))
  
  if(month_range == '001') {
    reclass_jan <-
      matrix(c(-Inf, 1, NA, 032, 366, 032, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_jan)
    
  } else if(month_range == '032') {
    reclass_feb <-
      matrix(c(-Inf, 1, NA, 1, 032, 032, 060, 366, 060, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_feb)
    
  } else if(month_range == '060') {
    reclass_march <-
      matrix(c(-Inf, 1, NA, 1, 060, 060, 091, 366, 091, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_march)
    
  } else if(month_range == '091') {
    reclass_april <-
      matrix(c(-Inf, 1, NA, 1, 091, 091, 121, 366, 121, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_april)
    
  } else if(month_range == '121') {
    reclass_may <-
      matrix(c(-Inf, 1, NA,  1, 121, 121, 152, 366, 152, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_may)
    
  } else if(month_range == '152') {
    reclass_june <-
      matrix(c(-Inf, 1, NA, 1, 152, 152, 182, 366, 182, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_june)
    
  } else if(month_range == '182') {
    reclass_july <-
      matrix(c(-Inf, 1, NA, 1, 182, 182, 213, 366, 213, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_july)
    
  } else if(month_range == '213') {
    reclass_august <-
      matrix(c(-Inf, 1, NA, 1, 213, 213, 244, 366, 244, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_august)
    
  } else if(month_range == '244') {
    reclass_september <-
      matrix(c(-Inf, 1, NA, 1, 244, 244, 274, 366, 274, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_september)
    
  } else if(month_range == '274') {
    reclass_october <-
      matrix(c(-Inf, 1, NA, 1, 274, 274, 305, 366, 305, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_october)
    
  } else if(month_range == '305') {
    reclass_november <-
      matrix(c(-Inf, 1, NA, 1, 305, 305, 335, 366, 335, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_november)
    
  } else if(month_range == '335') {
    reclass_december <-
      matrix(c(-Inf, 1, NA, 1, 335, 335, 367, Inf, NA),
             byrow = TRUE,
             ncol = 3)
    ras <- ras %>%
      raster::reclassify(reclass_december)
  }
  
  writeRaster(ras, file.path(output, outname), format = "GTiff", overwrite=TRUE)
  
}

tiles <- get_tiles(rim)
hdf_months <- file.path('data', 'testing', 'C6' , 'rim')
tif_months <- file.path('data', 'testing', 'C6' , 'rim')

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
  outname <- paste0(names, "_", filename, "_raw.tif")
  
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
        
        raw_dates(outname[M])
        clean_dates(outname[M])
        fill_dates(outname[M])
        
      }
    }
    setTxtProgressBar(pb, M)
  }
}
