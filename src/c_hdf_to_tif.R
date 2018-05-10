
# -----------------------------------------------------
source("src/a_prep_environment.R")

system("aws s3 sync s3://earthlab-natem/data/disturbances/fire/mcd64a1/tif_months data/MCD64A1/C6/tif_months")

clean_dates <- function(input_raster, output = tif_months) {
  outname <- gsub('_raw.tif', '_cleaned.tif', input_raster)
  month_range <- strsplit(input_raster, "\\_") %>%
    lapply(`[`, 2) %>%
    substr(6, 8)
  
  ras <- raster(file.path(output, input_raster))
  
  if(month_range == '001') {
    ras[ras < 1] <- NA
    ras[ras >= 032] <- NA
  } else if(month_range == '032') {
    ras[ras < 032] <- NA
    ras[ras >= 060] <- NA
  } else if(month_range == '060') {
    ras[ras < 060] <- NA
    ras[ras >= 091] <- NA
  } else if(month_range == '091') {
    ras[ras < 091] <- NA
    ras[ras >= 121] <- NA
  } else if(month_range == '121') {
    ras[ras < 121] <- NA
    ras[ras >= 152] <- NA
  } else if(month_range == '152') {
    ras[ras < 152] <- NA
    ras[ras >= 182] <- NA
  } else if(month_range == '182') {
    ras[ras < 182] <- NA
    ras[ras >= 213] <- NA
  } else if(month_range == '213') {
    ras[ras < 213] <- NA
    ras[ras >= 274] <- NA
  } else if(month_range == '274') {
    ras[ras < 274] <- NA
    ras[ras >= 305] <- NA
  } else if(month_range == '305') {
    ras[ras < 305] <- NA
    ras[ras >= 335] <- NA
  } else if(month_range == '335') {
    ras[ras < 335] <- NA
    ras[ras > 366] <- NA
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
    ras[ras < 1] <- NA
    ras[ras >= 032 & ras <= 366] <- 032
    ras[ras > 366] <- NA
    
  } else if(month_range == '032') {
    ras[ras < 1] <- NA
    ras[ras >= 1 & ras < 032] <- 033
    ras[ras >= 060 & ras <= 366] <- 060
    
  } else if(month_range == '060') {
    ras[ras < 1] <- NA
    ras[ras < 060] <- 061
    ras[ras >= 091 & ras <= 366] <- 091
    ras[ras > 366] <- NA
    
  } else if(month_range == '091') {
    ras[ras < 1] <- NA
    ras[ras < 091] <- 092
    ras[ras >= 121 & ras <= 366] <- 121
    ras[ras > 366] <- NA
    
  } else if(month_range == '121') {
    ras[ras < 1] <- NA
    ras[ras < 121] <- 122
    ras[ras >= 152 & ras <= 366] <- 152
    ras[ras > 366] <- NA
    
  } else if(month_range == '152') {
    ras[ras < 1] <- NA
    ras[ras < 152] <- 153
    ras[ras >= 182 & ras <= 366] <- 182
    ras[ras > 366] <- NA
    
  } else if(month_range == '182') {
    ras[ras < 1] <- NA
    ras[ras < 182] <- 183
    ras[ras >= 213 & ras <= 366] <- 213
    ras[ras > 366] <- NA
    
  } else if(month_range == '213') {
    ras[ras < 1] <- NA
    ras[ras < 213] <- 214
    ras[ras >= 274 & ras <= 366] <- 274
    ras[ras > 366] <- NA
    
  } else if(month_range == '274') {
    ras[ras < 1] <- NA
    ras[ras < 274] <- 275
    ras[ras >= 305 & ras <= 366] <- 305
    ras[ras > 366] <- NA
    
  } else if(month_range == '305') {
    ras[ras < 1] <- NA
    ras[ras < 305] <- 306
    ras[ras >= 335 & ras <= 366] <- 335
    ras[ras > 366] <- NA
    
  } else if(month_range == '335') {
    ras[ras < 1] <- NA
    ras[ras < 335] <- 336
    ras[ras <= 366 ] <- 366
    ras[ras > 366] <- NA
    
  }
  
  writeRaster(ras, file.path(output, outname), format = "GTiff", overwrite=TRUE)
  
}


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
        clean_dates(outname[M])
        fill_dates(outname[M])
        
      }
    }
    setTxtProgressBar(pb, M)
  }
}
