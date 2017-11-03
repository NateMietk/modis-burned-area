###################################################
# This script creates a layer of burn date for the#
# entire USA from MODIS collection 6 Burned area  #
# product (MCD64)                                 #
# Author: Adam Mahood                             #
# Latest Modification: March 2017                 #
###################################################

# Download from:
# ftp://fuoco.geog.umd.edu/db/MCD64A1/
# username: fire
# password: burnt

library(raster)
library(RCurl)
library(gdalUtils)

top_directory = "~/DATA/MODIS/"
output_directory = "~/DATA/MODIS/output/"
final_output = "~/DATA/MODIS/USA_BURNDATE/"

#optional
dir.create(paste0(top_directory))
dir.create(paste0(output_directory))
dir.create(paste0(final_output))


url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

reclassification_matrix_to_take_out_NAs = matrix(c(-Inf, 0, 0, 367, Inf, 0), byrow=TRUE, ncol=3)


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


# first, download the files

for(j in 1:length(tiles)){
  dir.create(paste0(top_directory, tiles[j]))
  setwd(paste0(top_directory, tiles[j]))
  filenames <- getURL(paste0(url,tiles[j],"/"), userpwd = u_p, v=T, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames = paste0(strsplit(filenames, "\r*\n")[[1]])
  for(L in 1:length(filenames)){
    download.file(paste0(url,tiles[j],"/",filenames[L]),paste0(top_directory,tiles[j],"/",filenames[L]))
  }
  
  # next, take the burn date rasters out of the .hdfs
  
  hdfs = dir(pattern = ".hdf")
  filename = paste(substr(hdfs, 10,16), substr(hdfs, 18, 23), sep="_")
  
  newfilename1 <- paste0("BurnDate", filename, ".tif")
  
  for (M in 1:length(hdfs)) {
    sds = get_subdatasets(hdfs[M])
    gdal_translate(sds[1], dst_dataset = newfilename1[M])
  }
  
  
  for(i in 2000:2016){
    tile_files = Sys.glob(paste("BurnDate",i,"*.tif", sep = ""))
    print(tile_files)
    
    one_stack_of_stuff = stack(tile_files)
    two_reclassified_stuff = reclassify(one_stack_of_stuff, reclassification_matrix_to_take_out_NAs)
    three_whole_year_of_fire = calc(two_reclassified_stuff, max)
    
    filename_for_the_tilefile = paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep="")
    
    writeRaster(three_whole_year_of_fire, filename_for_the_tilefile, format = "GTiff")
    
  }
  
  files_to_delete = list.files(paste0(top_directory,tiles[j],"/"))
  file.remove(files_to_delete)
}

######################## Then, stitch them all together

for(k in 2000:2016){
  
  setwd(output_directory)
  
  list_of_maxxed_tiles = as.vector(Sys.glob(paste0("*", k, ".tif")))
  
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
  
  
  whole_damn_country_filename = paste0(final_output,"USA_burn_date_courtesy_of_Adam_Mahood", k, ".tif")
  
  writeRaster(whole_damn_country, whole_damn_country_filename, format = "GTiff")
  
}

