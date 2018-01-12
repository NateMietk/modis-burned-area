
# Download all .hdf files from ftp site for the Conterminous US
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

# Convert .hdfs to .tifs, project to albers equal area, and reclassify to just julian date
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
      r <- raster::raster(sds$SDS4gdal[d]) %>%
        raster::reclassify(., mtrx)
      
      if(!file.exists(paste0(top_directory, tiles[j], "/", newfilename1[M]))) {
        writeRaster(r, paste0(top_directory, tiles[j], "/", newfilename1[M]), overwrite=TRUE)
      }
    }
  }
}

# Create yearly composites for all tiles
for(i in 2001:2017){
  tile_files = list.files(paste0(top_directory, tiles[j]), 
                          pattern = "*.tif$", full.names=TRUE)
  if(!file.exists(paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep=""))){
    fire = raster::stack(tile_files) %>%
      calc(., max)
    
    tfilename = paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep="")
    writeRaster(fire, tfilename, format = "GTiff")
  }
  #}
  # files_to_delete = list.files(paste0(top_directory,tiles[j],"/"))
  # file.remove(files_to_delete)
}

# Create mosaic of burned area for the lower 48 US for each year
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
      final <- do.call(merge, r.list) %>%
        raster::crop(as(wus_ms, "Spatial")) %>%
        raster::mask(as(wus_ms, "Spatial")) %>%
        raster::projectRaster(crs = p4string_ea, res = 500)
      
      final_name <- paste0(final_output,"USA_", names[d], k, ".tif")
      writeRaster(final, final_name, format = "GTiff")
    }
  }
}
