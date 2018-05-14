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
modis_proj <- p4string_ea
mtbs <- st_read(mtbs_shp) %>%
  st_intersection(., st_union(st_transform(usa, st_crs(.)))) %>%
  st_transform(crs = modis_proj)


# running the numbers -----------------------------------  
x = list.files("data/yearly_events/")
years <- as.numeric(unlist(regmatches(x, gregexpr("\\d{4}", x))))
#note - used capital letters to exactly match mtbs data if we want to join in the future

results <- data.frame(Fire_ID = NA, 
                      modis_id_nobuff=NA, 
                      modis_id_buff =NA, 
                      n_nobuff = NA, 
                      n_buff = NA, 
                      Acres = NA,
                      Year = NA) #make a field with modis acres
counter <- 1
for(y in 1:length(years)){
  modis_y <- raster(paste0("data/yearly_events/USA_burnevents_",years[y],".tif"))
  modis_y <- modis_y + as.numeric(paste0(years[y],"00000"))
  mtbs_y <- mtbs[mtbs$Year == years[y],] 
  for(f in 1:nrow(mtbs_y)){
    fire <- mtbs_y[f,]
    fire_b <- st_buffer(fire, dist = 917) #native resolution is 463
    cropped <- raster::crop(modis_y, as(fire, "Spatial"))
    masked <- raster::mask(cropped, as(fire, "Spatial"))
    cropped_b <- raster::crop(modis_y, as(fire_b, "Spatial"))
    masked_b <- raster::mask(cropped_b, as(fire_b, "Spatial"))
    vc <- unique(getValues(masked))
    vc <- vc[vc>0]
    vb <- unique(getValues(masked_b))
    vb <- vb[vb>0]
    
    vc <- ifelse(vc == as.numeric(paste0(years[y],"00000")), NA, vc)
    vb <- ifelse(vb == as.numeric(paste0(years[y],"00000")), NA, vb)
    
    if(length(vc) == 0){vc<-NA}
    if(length(vm) == 0){vm<-NA}
    
    
    results[counter, 1] <- as.character(fire$Fire_ID)
    results[counter, 2] <- paste(as.character(vc[vc>0 & !is.na(vc)]), collapse = " ")
    results[counter, 3] <- paste(as.character(vb[vb>0 & !is.na(vb)]), collapse = " ")
    results[counter, 4] <- length(vc[vc>0 & !is.na(vc)])
    results[counter, 5] <- length(vb[vb>0 & !is.na(vb)])
    results[counter, 6] <- fire$Acres
    results[counter, 7] <- fire$Year
    counter <- counter + 1
  }
}

# breaking it down to just mtbsIDs and modis IDs ------------------
long_mt_mo <- data.frame(Fire_ID=NA, modis_id_b=NA)
counter <- 1
for(i in 1:nrow(results)){
  ss <- strsplit(results$modis_id_buff[i], " ") %>% unlist()
  if(length(ss)>0){for(j in 1:length(ss)){
    long_mt_mo[counter, 1] <- results$Fire_ID[i]
    long_mt_mo[counter, 2] <- ss[j]
    counter <- counter + 1
  }}else{
    long_mt_mo[counter, 1] <- results$Fire_ID[i]
    long_mt_mo[counter, 2] <- ss[j]
    counter <- counter + 1}
}

# calculate modis id numbers -----------------------------------
tabs <-  list()
vals <- list()
m_ids <- data.frame(year = NA, n_ids = NA)
for(i in 1:length(years)){
  modis_y <- raster(paste0("data/yearly_events/USA_burnevents_",years[i],".tif"))
  modis_y <- modis_y + as.numeric(paste0(years[i],"00000"))
  vals[[i]] <- getValues(modis_y) %>%
    na.omit()
  vals[[i]] <- vals[[i]][vals[[i]]>0]
  vals[[i]] <- ifelse(vals[[i]] == as.numeric(paste0(years[y],"00000")), NA, vals[[i]]) %>% na.omit()
  m_ids[i,1] <- years[i]
  m_ids[i,2] <- length(unique(vals[[i]]))
  tabs[[i]] <- as.data.frame(table(vals[[i]])) %>% rename(modisID = Var1)
}
over200ha <- do.call("rbind", tabs) %>%
  filter(Freq >20)
# todo - remove fires less than 20 pixels (200 ha)
# then join to mtbs IDs

# 
# ids_matching <- results$modis_IDs_buff %>%
#   strsplit(" ") %>%
#   unlist() %>%
#   unique()

# confusion matrix without removing small fires----------------------------------

confusion_matrix <- data.frame(modis = c("modis_ID_T", "modis_ID_F"), mtbs_ID_T = NA, mtbs_ID_F = NA)
confusion_matrix[1,2] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id_b),]$Fire_ID))
confusion_matrix[2,2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id_b),]$Fire_ID))
confusion_matrix[1,3] <- sum(m_ids$n_ids) - confusion_matrix[1,2]

# confusion matris after removing small fires
confusion_matrix2 <- data.frame(modis = c("modis_ID_T", "modis_ID_F"), mtbs_ID_T = NA, mtbs_ID_F = NA)
confusion_matrix2[1,2] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id_b),]$Fire_ID))
confusion_matrix2[2,2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id_b),]$Fire_ID))
confusion_matrix2[1,3] <- nrow(over200ha) - confusion_matrix[1,2]

# number of mtbs fire with multiple modis IDs ------------------------------------
t <- table(results$n_buff)
mtbs_w_multiple_modis <- sum(t[3:length(t)])

