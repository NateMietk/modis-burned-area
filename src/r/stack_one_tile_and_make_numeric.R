# this is just making a 3d array out of one modis tile (h09v05)

library(tidyverse)
library(foreach)
library(doParallel)
library(raster)
library(gdalUtils)

# aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/hdf_months/ /home/a/projects/modis-burned-area/scrap/ --recursive --exclude "*" --include "*h09v05*"

in_dir <- "/home/a/projects/modis-burned-area/scrap/hdf"
out_dir <-  "/home/a/projects/modis-burned-area/scrap/tif_months"
out_dir_1 <- "/home/a/projects/modis-burned-area/scrap/tif_converted"
res_dir <- "/home/a/projects/modis-burned-area/scrap/result"

hdfs <- list.files(in_dir,
                   recursive = TRUE, full.names = TRUE)

filename <- strsplit(basename(hdfs), "\\.") %>%
  lapply(`[`, 2:3) %>%
  lapply(paste, collapse = "_") %>%
  unlist

# create the final name to be written out
outname <- paste0("bd_test", "_", filename, ".tif")

n_cores <- detectCores()-1
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)

foreach (i = 1:length(hdfs), .packages = c('gdalUtils', 'foreach')) %dopar% {
  
  out_file <- paste0(out_dir, "/", outname[i])
  
  if(!file.exists(out_file)) {
    # get the subdatasets from the hdf file
    sds <- gdalUtils::get_subdatasets(hdfs[i])
    gdalUtils::gdal_translate(sds[1], dst_dataset = out_file)
  }
}
parallel::stopCluster(cl)

# convert the tifs and make one big stack to write out -------------------------
tifs <- list.files(out_dir,
                   recursive = TRUE, full.names = TRUE)

from_r_origin <- as.Date("1999365", "%Y%j") %>% as.numeric

leap_years <- c(2000,2004,2008,2012,2016)
years_days <- data.frame(year = 2000:2019, days = 365) %>%
  mutate(days = replace(days, year %in% leap_years, 366),
         prior = lag(days),
         prior = replace(prior, is.na(prior)==T, 0),
         cum = cumsum(prior),
         from_origin = cum+from_r_origin)

for(i in 1:length(tifs)){
  year <-substr(tifs[i], 62,65) %>% as.numeric
  days <-substr(tifs[i], 66,68) %>% as.numeric
  
  days_to_add <- years_days[years_days$year == year, "from_origin"]
  
  
  x <- raster(tifs[i])
  x[x < 1] <- NA
  x[x > 366] <- NA
  
  y=x+days_to_add
  writeRaster(y, paste0(out_dir_1, "/bd_numeric_",year, "_",days,".tif"), overwrite=T)
  print(i/length(tifs))
}


# # writing the stack to a single .tif takes a long time and might be unnecessary

tifs_1 <- list.files(out_dir_1,
                     recursive = TRUE,
                     full.names = TRUE)
rl <- list()
for(i in 1:length(tifs_1)){ rl[[i]] <- raster(tifs_1[i])}
stk <- raster::stack(rl)
writeRaster(stk, paste0(res_dir,"/h09v05_full_stack.nc"))
