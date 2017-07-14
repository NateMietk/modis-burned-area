library(raster)

# This script creates a layer of burn date for the entire USA from MODIS collection 6 Burned area product (MCD64)
# Download from:
# ftp://fuoco.geog.umd.edu/db/MCD64A1/
# username: fire
# password: burnt
# This script assumes you already downloaded the MCD64 Fire data into separate folders for each tile
# and took out the burn date layer from each hdf file.


# modify this path to be directory that the tile folders sit in

top_directory = "~/DATA/MODIS/"
output_directory = "~/DATA/MODIS/output/"

reclassification_matrix_to_take_out_NAs = matrix(c(-Inf, 0, 0, 367, Inf, 0), byrow=TRUE, ncol=3)

#assuming tile folders are named in the same format as the folder they were downloaded from

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


for(i in 2000:2015){
  
  for(j in 1:length(tiles)){
    
    
    setwd(paste(top_directory, tiles[j], sep=""))
    
    tile_files = Sys.glob(paste("BurnDate",i,"*.tif", sep = ""))
    
    print(tile_files)
    
    one_stack_of_stuff = stack(tile_files)
    two_reclassified_stuff = reclassify(one_stack_of_stuff, reclassification_matrix_to_take_out_NAs)
    three_whole_year_of_fire = calc(two_reclassified_stuff, max)
    
    filename_for_the_tilefile = paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep="")
    
    writeRaster(three_whole_year_of_fire, filename_for_the_tilefile, format = "GTiff")
    
  }
  
}

for(k in 2000:2015){
  
  setwd(output_directory)
  
  list_of_maxxed_tiles = as.vector(Sys.glob(paste("*", k, ".tif", sep="")))
  
  #maybe there's a way to do this next step without typing out all the tiles... perhaps raster::stack?
  
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
                             
  whole_damn_country_filename = paste("USA_Burn_Date", k, ".tif", sep = "")
  
  writeRaster(whole_damn_country, whole_damn_country_filename, format = "GTiff")
  
}

