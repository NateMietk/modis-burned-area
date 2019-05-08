rst_list <- list.files(yearly_events_dir, pattern = 'events', full.name = TRUE)

cl <- parallel::makeCluster(6)
doParallel::registerDoParallel(cl)
ms_rst <- pbapply::pblapply(rst_list, FUN = function(x, out_dir, out_s3){
  require(tidyverse)
  require(raster)
  filename <- x %>%
    basename() %>%
    str_split(., '\\.') %>%
    unlist()
  filename <- filename[1]
  
  if(!file.exists(file.path(out_dir, paste0(filename, '_ms.tif')))) {
    rst <- raster::raster(x) %>%
      raster::projectRaster(., crs = '+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs ',
                            res = 463.3127, 
                            method = 'ngb') 
    writeRaster(rst, file.path(out_dir, paste0(filename, '_ms.tif')), overwrite = TRUE)
    # system(paste0('aws s3 sync ', out_dir, ' ', out_s3, '/', out_dir))
  }
}, cl=cl, out_dir = yearly_composites, out_s3 = s3_base)
parallel::stopCluster(cl)
