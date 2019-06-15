# confusion matrix 2019

# setup ========================================================================
libs<-c("tidyverse", "sf","raster","units")
lapply(libs,library, character.only=T)

template_path <- "/home/a/data/MCD12Q1_mosaics"
cus_path <- "/home/a/data/background/CUS"

unnecessary <- function(x) {
  # thanks http://rpubs.com/sogletr/sf-ops
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# loading and wrangling data ===================================================
template <- raster(file.path(template_path, "usa_lc_mosaic_2001.tif"))


modis <- st_read("data/events_w_attributes_cus.gpkg") %>%
  filter(ignition_date < as.Date("2016-01-01"))

cus <- st_read(cus_path) %>%
  summarise() %>%
  st_transform(st_crs(modis))

mtbs <- st_read("/home/a/data/fire/mtbs/mtbs_perims_DD.shp") %>%
  filter(Year < 2016) %>%
  st_transform(crs = st_crs(modis))
  
x2<- st_intersects(mtbs, cus) %>%
  map_lgl(unnecessary)

mtbs_cus <- mtbs[x2,] 
st_write(mtbs_cus, "data/mtbs_cus_uncasted.gpkg")

mtbs_cus <- st_read("data/mtbs_cus_uncasted.gpkg")
mtbs_cus <- mtbs_cus %>%
  st_cast(to = "MULTIPOLYGON") %>%
  st_cast(to = "POLYGON")

mtbs_cus$duped <- duplicated(mtbs_cus$Fire_ID)
mtbs_cus$new_id <- ifelse(mtbs_cus$duped == TRUE,
                      paste(as.character(mtbs_cus$Fire_ID),
                            as.character(row_number(mtbs_cus$Fire_ID)), 
                            sep="_"),
                      as.character(mtbs_cus$Fire_ID))
mtbs_cus$cast_area_ha <- st_area(mtbs_cus[0])%>% set_units(value = hectare)
mtbs_cus$cast_area_ac <- st_area(mtbs_cus[0])%>% set_units(value = acre)

mtbs_y <- mtbs[mtbs$Year == years[y],] 


# this is the part where we extract information for each mtbs polygon
for(f in 1:nrow(mtbs_y)){
  fire <- mtbs_y[f,]
  cropped <- raster::crop(modis_y, as(fire, "Spatial"), snap = 'out')
  SpP_ras <- rasterize(fire, cropped, getCover=TRUE)
  SpP_ras[SpP_ras==0] <- NA
  SpP_ras[SpP_ras > 1] <- 1
  masked <- cropped * SpP_ras
  
  vc <- unique((masked[masked>0]))
  vc <- vc + as.numeric(paste0(years[y],"00000"))
  if(length(vc) == 0){vc<-NA}
  
  
  bpix_nobuff <- table(getValues(masked)) 
  barea_ha_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 21.4369
  barea_ac_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 52.9717335
  
  
  w_e <-as.character(ifelse(st_bbox(st_transform(fire,4326))[1] < -97, "w", "e"))
  
  results[counter, 1] <- as.character(fire$Fire_ID)
  results[counter, 2] <- as.character(fire$new_id)
  results[counter, 3] <- paste(as.character(vc), collapse = " ")
  results[counter, 4] <- length(vc[!is.na(vc)])
  results[counter, 5] <- fire$cast_area_ac
  results[counter, 6] <- fire$cast_area_ha
  results[counter, 7] <- fire$Year
  results[counter, 8] <- barea_ha_nobuff
  results[counter, 9] <- barea_ac_nobuff
  results[counter, 10] <- w_e
  print(c(counter, years[y]))
  counter <- counter + 1
}