
# Download from:
# ftp://fuoco.geog.umd.edu/MCD64A1/C6/
# username: fire
# password: burnt
x <- c("MODIS", "tidyverse", "magrittr", "raster", "RCurl", "gdalUtils", "foreach", "doParallel")
lapply(x, library, character.only = TRUE, verbose = FALSE)

top_directory = paste0(file.path("data", "MCD64A1", "C6"), "/")
output_directory = paste0(file.path("data", "MCD64A1", "C6", "yearly_tiles"), "/")
final_output = paste0(file.path("data", "MCD64A1", "C6", "usa_burndate"), "/")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(top_directory, output_directory, final_output)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

# Reclassification matrix to remove NA values
mtrx = matrix(c(-Inf, 0, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
tiles = c("h08v04", "h08v05", "h09v04", "h09v05", "h10v04", "h10v05",
          "h10v06", "h11v04", "h11v05", "h12v04", "h12v05", "h13v04")

names <- c("BurnDate")
layers <- c("Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day")

for(j in 1:length(tiles)){
  dir.create(paste0(top_directory, tiles[j]), recursive = TRUE)
  filenames <- getURL(paste0(url,tiles[j],"/"), userpwd = u_p, v=T, ftp.use.epsv = FALSE, dirlistonly = TRUE)
  filenames = paste0(strsplit(filenames, "\r*\n")[[1]])
  for(L in 1:length(filenames)){
    output_file_name <- file.path(paste0(top_directory,tiles[j]), filenames[L])
    if(!file.exists(output_file_name)) {
      download.file(paste0(url,tiles[j],"/",filenames[L]),
                    output_file_name)
    } 
  }
}

# next, take the burn date rasters out of the .hdfs
for(d in 1){ 
  for(j in 1:length(tiles)){    
    
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
    
    for(i in 2001:2017){
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
}

# Then, stitch them all together

for(d in 1) {
  for(k in 2016:2017){
    output_directory <- file.path("data", "MCD64A1", "C6", "yearly_tiles")
    
    tile_files = list.files(output_directory, 
                            pattern = "*.tif$", full.names=TRUE)
    if(!file.exists(paste0(final_output,"USA_", names[d], k, ".tif"))){
      
      r.list <- list()
      for(N in 1:length(tiles)){  
        r.list[[N]] <- raster(paste0(output_directory, "/AllYear_BD_", tiles[N], "_", k, ".tif"))  
      } 
      final <- do.call(merge, r.list)
      
      final_name <- paste0(final_output,"USA_", names[d], k, ".tif")
      writeRaster(final, final_name, format = "GTiff")
    }
  }
}
