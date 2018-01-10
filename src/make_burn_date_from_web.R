###################################################
# This script creates a layer of burn date for the#
# entire USA from MODIS collection 6 Burned area  #
# product (MCD64)                                 #
# Author: Adam Mahood                             #
# Latest Modification: March 2017                 #
###################################################

# Download from:
# ftp://fuoco.geog.umd.edu/MCD64A1/C6/
# username: fire
# password: burnt

# filenames from subdatastes
## "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:Burn Date" 
# [1] "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:Burn Date"            
# [2] "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:Burn Date Uncertainty"
# [3] "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:QA"                   
# [4] "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:First Day"            
# [5] "HDF4_EOS:EOS_GRID:MCD64A1.A2014213.h10v04.051.2014284031847.hdf:MOD_Grid_Monthly_500m_DB_BA:Last Day" 

library(MODIS)
library(tidyverse)
library(raster)
library(RCurl)
library(gdalUtils)
library(foreach)
library(doParallel)

top_directory = "data/MODIS/"
output_directory = "data/MODIS/output/"
final_output = "data/MODIS/USA_BURNDATE/"

#optional
dir.create(paste0(top_directory), recursive = TRUE)
dir.create(paste0(output_directory), recursive = TRUE)
dir.create(paste0(final_output), recursive = TRUE)


url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

# Reclassification matrix to remove NA values
mtrx = matrix(c(-Inf, 0, 0, 367, Inf, 0), byrow=TRUE, ncol=3)

tiles = c("h08v04", 
          "h08v05", 
          "h09v04", 
          "h09v05", 
          "h10v04",
          "h10v05",
          "h10v06",
          "h11v04",
          "h11v05",
          "h12v04",
          "h12v05",
          "h13v04")

names <- c("BurnDate", "BurnDateUncertainty")

layers <- c("Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day")

#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores) #not to overload your computer
registerDoParallel(cl)

foreach(j = 1:length(tiles)) %dopar% {
  dir.create(paste0(top_directory, tiles[j]), recursive = TRUE)
  filenames <- RCurl::getURL(paste0(url,tiles[j],"/"), userpwd = u_p, v=T, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames = paste0(strsplit(filenames, "\r*\n")[[1]])
  for(L in 1:length(filenames)){
    output_file_name <- file.path(paste0(top_directory,tiles[j]), filenames[L])
    if(!file.exists(output_file_name)) {
      download.file(paste0(url,tiles[j],"/",filenames[L]),
                    output_file_name)
    } 
  }
}
#stop cluster
stopCluster(cl)

# next, take the burn date rasters out of the .hdfs
registerDoParallel(cl)
foreach (d = 1:2) %:% # nesting operator
  foreach (j = 1:length(tiles)) %dopar% {
    hdfs = list.files(paste0(top_directory, tiles[j]), pattern = ".hdf", 
                      recursive = TRUE)
    
    filename = strsplit(hdfs, "\\.") %>%
      lapply(`[`, 2:3) %>%
      lapply(paste, collapse = "_") %>%
      unlist
    
    newfilename1 <- paste0(names[d], filename, ".tif")
    
    hdfs_full = list.files(paste0(top_directory, tiles[j]), pattern = ".hdf", 
                           recursive = TRUE, full.names = TRUE)
    
    for (M in 1:length(hdfs_full)) {
      sds <- getSds(hdfs_full[M])
      r <- raster(sds$SDS4gdal[d])
      if(!file.exists(paste0(top_directory, tiles[j], "/", newfilename1[M]))) {
        writeRaster(r, paste0(top_directory, tiles[j], "/", newfilename1[M]))
      }
    }
    
    for(i in 2000:2017){
      tile_files = list.files(paste0(top_directory, tiles[j]), 
                              pattern = "*.tif$", full.names=TRUE)
      if(!file.exists(paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep=""))){
        rst_stk = raster::stack(tile_files)
        rcls = reclassify(rst_stk, mtrx)
        fire = calc(rcls, max)
        
        tfilename = paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep="")
        
        writeRaster(fire, tfilename, format = "GTiff")
      }
    }
    files_to_delete = list.files(paste0(top_directory,tiles[j],"/"))
    file.remove(files_to_delete)
  }
stopCluster(cl)

######################## Then, stitch them all together

for(d in 1:2) {
  for(k in 2000:2017){
    
    list_of_maxxed_tiles = as.vector(Sys.glob(paste0(output_directory, "*", k, ".tif")))
    
    whole_damn_country = raster::merge(raster(paste("AllYear_BD_h08v04_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h08v05_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h09v04_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h09v05_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h10v04_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h10v05_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h10v06_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h11v04_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h11v05_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h12v04_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h12v05_",k,".tif", sep = "")),
                                       raster(paste("AllYear_BD_h13v04_",k,".tif", sep = "")))
    
    
    ### need to find a more generic way to merge, it's in the help file for raster::merge
    # whole_damn_country = sapply()
    #   raster::merge(
    #   for(N in 1:length(tiles)){
    #     raster(paste0("AllYear_BD_",tiles[N],"_",k,".tif"))
    #   }
    # )
    
    
    whole_damn_country_filename = paste0(final_output,"USA_", names[d], k, ".tif")
    
    writeRaster(whole_damn_country, whole_damn_country_filename, format = "GTiff")
  }
}

