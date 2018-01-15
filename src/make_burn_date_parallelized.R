
# Download all .hdf files from ftp site for the Conterminous US
cl <- makeCluster(cores)
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
stopCluster(cl)

# Convert .hdfs to .tifs, project to albers equal area, and reclassify to just julian date
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach (d = 1) %:% # nesting operator
  foreach (j = 1:length(tiles)) %dopar% {
    require(magrittr)
    require(MODIS)
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
      sds <- MODIS::getSds(hdfs_full[M])
      r <- raster::raster(sds$SDS4gdal[d]) %>%
        raster::reclassify(., mtrx) 
      #if(!file.exists(paste0(top_directory, tiles[j], "/", newfilename1[M]))) {
      raster::writeRaster(r, paste0(top_directory, tiles[j], "/", newfilename1[M]), overwrite=TRUE)
      #}
    }
  }
stopCluster(cl)

# Create yearly composites for all tiles
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach (j = 1:length(tiles)) %:% # nesting operator
  foreach (i = 2016:2017) %dopar% {
    require(magrittr)
    tile_files = list.files(paste0(top_directory, tiles[1]),
                            pattern = "*.tif$", full.names=TRUE)
    #if(!file.exists(paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep=""))){
    fire = raster::stack(tile_files) %>%
      calc(., max)
    
    tfilename = paste(output_directory, "AllYear_BD_", tiles[j], "_", i, ".tif", sep="")
    
    raster::writeRaster(fire, tfilename, format = "GTiff")
    #}
    # files_to_delete = list.files(paste0(top_directory,tiles[j],"/"))
    # file.remove(files_to_delete)
  }
stopCluster(cl)

# Create mosaic of burned area for the lower 48 US for each year
cl <- makeCluster(cores)
registerDoParallel(cl)
foreach(d = 1) %:% # nesting operator
  foreach(k = 2016:2017) %dopar% {
    output_directory <- file.path("data", "MCD64A1", "C6", "yearly_tiles")
    
    tile_files = list.files(output_directory, 
                            pattern = "*.tif$", full.names=TRUE)
    if(!file.exists(paste0(final_output,"USA_", names[d], k, ".tif"))){
      
      r.list <- list()
      for(N in 1:length(tiles)){  
<<<<<<< HEAD
        
=======

>>>>>>> 8b2cb4cb916cad65adfff8d6ec18ff0dbf53a415
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
stopCluster(cl)

