source("src/a_prep_environment.R")

# import files ------------------------------------------------------------------------------

system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_events data/yearly_events")

mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
}

# next thing
modis_proj <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
mtbs <- st_read(mtbs_shp) %>%
  st_intersection(., st_union(st_transform(usa, st_crs(.)))) %>%
  st_transform(crs = modis_proj)
  

years <- 2001:2015

results <- data.frame(Fire_ID = NA, modis_IDs_c=NA, modis_IDs_m =NA, n_c = NA, n_m = NA, Acres = NA)
counter <- 1
for(y in 1:length(years)){
  modis_y <- raster(paste0("data/yearly_events/USA_burnevents_",years[y],"_filled.tif"))
  mtbs_y <- mtbs[mtbs$Year == years[y],] 
  for(f in 1:nrow(mtbs_y)){
    fire <- mtbs_y[f,]
    cropped <- raster::crop(modis_y, as(fire, "Spatial"))
    masked <- raster::mask(cropped, as(fire, "Spatial"))
    vc <- unique(getValues(cropped))
    vc <- vc[vc>0]
    vm <- unique(getValues(masked))
    vm <- vm[vm>0 & !is.na(vm)]
    
    if(length(vc) == 0){vc<-NA}
    if(length(vm) == 0){vm<-NA}
    
    
    results[counter, 1] <- as.character(fire$Fire_ID)
    results[counter, 2] <- paste(as.character(vc), collapse = " ")
    results[counter, 3] <- paste(as.character(vm), collapse = " ")
    results[counter, 4] <- length(vc[vc>0 & !is.na(vm)])
    results[counter, 5] <- length(vm[vm>0 & !is.na(vm)])
    results[counter, 6] <- fire$Acres
    counter <- counter + 1
  }
}

