# confusion matrix 2019

# setup ========================================================================
libs<-c("tidyverse", "sf","raster","units", "foreach", "doParallel")
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

modis_w <- st_read("data/events_w_attributes_cus.gpkg") %>%
  filter(ignition_date < as.Date("2016-01-01")) %>%
  st_transform(4326) %>%
  filter(st_coordinates(st_centroid(modis)[0] )[,1] < -97,
         total_area_acres >1000)

modis_e <- st_read("data/events_w_attributes_cus.gpkg") %>%
  filter(ignition_date < as.Date("2016-01-01")) %>%
  st_transform(4326) %>%
  filter(st_coordinates(st_centroid(modis)[0] )[,1] > -97,
         total_area_acres >500)

modis_over_th <- rbind(modis_w, modis_e) %>%
  st_transform(crs = st_crs(modis))

nrow(modis_over_th)
nrow(modis) - nrow(modis_over_th)

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

cm_df <- data.frame(year = NA, mtbsTmodisT = NA, modisTmtbsT = NA,
                    mtbsTmodisF = NA, modisTmtbsF = NA, modisTmtbsF_over_th = NA)
years <- 2001:2015

for(y in 1:length(years)){
  mtbs_y <- mtbs_cus[mtbs_cus$Year == years[y],] 
  modis_y <- modis[modis$ignition_year == years[y],] 
  modis_o_y <- modis_over_th[modis_over_th$ignition_year == years[y],] 
  
  x2<- st_intersects(mtbs_y, modis_y) %>%
    map_lgl(unnecessary)
  x3<- st_intersects(modis_y, mtbs_y) %>%
    map_lgl(unnecessary)
  
  cm_df[y, 1] <- years[y]
  cm_df[y, 2] <- nrow(mtbs_y[x2,])
  cm_df[y, 3] <- nrow(modis_y[x3,])
  cm_df[y, 4] <- nrow(mtbs_y) - nrow(mtbs_y[x2,])
  cm_df[y, 5] <- nrow(modis_y) - nrow(mtbs_y[x2,])
  cm_df[y, 6] <- nrow(modis_o_y) - nrow(mtbs_y[x2,])
  
  
  
}
sum(cm_df$mtbsTmodisT)
sum(cm_df$modisTmtbsT)
sum(cm_df$mtbsTmodisF)
sum(cm_df$modisTmtbsF)
sum(cm_df$modisTmtbsF_over_th)
sum(cm_df$modisTmtbsF)-sum(cm_df$modisTmtbsF_over_th)

df<- data.frame(x = c("FIRED_True", "FIRED_False"),
                "MTBS_True" = c(sum(cm_df$mtbsTmodisT),sum(cm_df$mtbsTmodisF)),
                "MTBS_False" = sum(cm_df$modisTmtbsF),
                "MTBS_False" = sum(cm_df$modisTmtbsF)-sum(cm_df$modisTmtbsF_over_th)
                ) %>%
  kable(booktabs = TRUE) %>%
  kable_styling(bootstrap_options = "basic") %>%
  save_kable("kabletest.png")


df <- read_csv("data/modis_burn_events_00_19.csv") %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2),
         year = as.numeric(substr(date, 1,4))) %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE))

# same as 2018 but modified
results <- data.frame(Fire_ID = NA, # this is the original mtbs ID
                      mtbs_cast_id = NA, # this is the modified ID in the case of multipolygons
                      modis_id = NA, 
                      n = NA, # number of modis ids within the mtbs polygon
                      mtbs_acres = NA,
                      mtbs_hectares = NA,
                      mtbs_year = NA,
                      modis_ha = NA,
                      modis_acres = NA,
                      west_or_east = NA)
# this is the part where we extract information for each mtbs polygon
corz <-detectCores()-1
registerDoParallel(corz)

p_results <- foreach(y = 1:length(years), .combine = "rbind")%dopar%{
  mtbs_y <- mtbs_cus[mtbs_cus$Year == years[y],] 
  modis_y <- df[df$year == years[y],]
  
  results <- data.frame(Fire_ID = NA, # this is the original mtbs ID
                        mtbs_cast_id = NA, # this is the modified ID in the case of multipolygons
                        modis_id = NA, 
                        n = NA, # number of modis ids within the mtbs polygon
                        mtbs_acres = NA,
                        mtbs_hectares = NA,
                        mtbs_year = NA,
                        modis_ha = NA,
                        modis_acres = NA,
                        west_or_east = NA)
  counter <- 1
  for(f in 1:nrow(mtbs_y)){
    
    fire <- mtbs_y[f,]
    
    modis_in_fire <- st_intersection(modis_y, fire)
    
    # getting the number of event ids
    vc <- unique((modis_in_fire$id))
    if(length(vc) == 0){vc<-NA}
    
    
    bpix_nobuff <- nrow(modis_in_fire) 
    barea_ha_nobuff <- bpix_nobuff * (463.3127^2) * .0001
    barea_ac_nobuff <- bpix_nobuff * (463.3127^2) * 0.000247105
    
    
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
    counter <- counter +1
    system(paste("echo",years[y],f/nrow(mtbs_y)*100, "%"))
  }
  return(results)
}

write_csv(p_results,"data/cm_2019.csv")


longfile = paste0("long_cast_2019.csv")
if(!file.exists(paste0("data/",longfile))){
  long_mt_mo <- data.frame(Fire_ID=NA, mtbs_cast_id=NA, modis_id=NA)
  counter <- 1
  for(i in 1:nrow(results)){
    ss <- strsplit(results$modis_id[i], " ") %>% unlist()
    if(length(ss)>0){for(j in 1:length(ss)){
      long_mt_mo[counter, 1] <- results$Fire_ID[i]
      long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
      long_mt_mo[counter, 3] <- ss[j]
      counter <- counter + 1
    }}else{
      long_mt_mo[counter, 1] <- results$Fire_ID[i]
      long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
      long_mt_mo[counter, 3] <- NA
      counter <- counter + 1}
  }
  gc() # doing this as much as possible to conserve resources
  
  
  
  write_csv(long_mt_mo, paste0("data/",longfile))
 
}else{long_mt_mo <- read_csv(paste0("data/",longfile), stringsAsFactors = FALSE)}
# calculate modis id numbers -----------------------------------
e_th <- 202/21.4369 #thresholds in pixels (hectares)
w_th <- 404/21.4369

# this is calculating the area of each modis fire event
m_ids <- data.frame(year = NA, n_ids = NA, n_over_th = NA)
for(i in 1:length(years)){
  print(years[i])

  r <- df %>% filter(year ==years[i])
  m_ids[i,1] <- years[i]
  m_ids[i,2] <- length(unique(r$id))
  print("freq")
  
  r[r<1] <- NA
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  d <- rasterToPoints(r, spatial=TRUE) %>%
    spTransform(CRS(geo.prj))
  d <- data.frame(d@data, long=coordinates(d)[,1]) 
  names(d) <- c("a", "long")
  d <- as_tibble(d) %>%
    group_by(a) %>%
    summarize(pixel_count = n(),
              long = mean(long))
  d$th <- ifelse(d$long < -97, w_th,e_th)
  
  m_ids[i,3] <- nrow(d[d$th < d$pixel_count, ])
  print("over_th")
}
dir.create("data/m_ids/")
write.csv(m_ids, paste0("data/m_ids/m_ids_s",SS,"t",TT,".csv"))
# big table time baby ----------------------------------

big_table <- data.frame(modisT_mtbsT = NA,
                        modisF_mtbsT = NA,
                        modisT_mtbsF_all_modis = NA,
                        modisT_mtbsF_modis_over_th = NA,
                        st_combo = NA,
                        mtbs_w_multiple_modis = NA,
                        modis_w_multiple_mtbs = NA,
                        mean_n_modis_per_mtbs = NA,
                        median_n_modis_per_mtbs = NA,
                        max_n_modis_per_mtbs = NA,
                        which_max_modis_per_mtbs = NA,
                        mean_n_mtbs_per_modis = NA,
                        median_n_mtbs_per_modis = NA,
                        max_n_mtbs_per_modis = NA,
                        which_max_mtbs_per_modis = NA,
                        row_check = NA,
                        mtbsT_modisT_unique_modis_events = NA,
                        mtbsT_modisT_total_n_modis_events_with_repeats = NA,
                        space = NA,
                        time = NA,
                        mtbs_IDs_of_max_modis = NA)

rc = table(results$n) %>% as_tibble()
NN <- sum(rc$n[1:2])
rc <- rc[3:nrow(rc),]
for(ROW in 1:nrow(rc)){
  XX <- as.numeric(rc$Var1[ROW])
  NN <- NN + (XX * rc$n[ROW])
}

n_modis_per_mtbs <- table(long_mt_mo$mtbs_cast_id) %>% 
  as_tibble() %>%
  filter(Var1 != "NA")

n_mtbs_per_modis <- table(long_mt_mo$modis_id) %>% 
  as_tibble() %>%
  filter(Var1 != "NA")

big_table[1, 1] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
big_table[1, 2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
big_table[1, 3] <- sum(m_ids$n_ids, na.rm=T) - length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
big_table[1, 4] <- sum(m_ids$n_over_th, na.rm=T) - length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))

big_table[1, 5] <- paste0("s",SS,"t",TT)
big_table[1, 6] <- nrow(n_modis_per_mtbs[n_modis_per_mtbs$n > 1,])
big_table[1, 7] <- nrow(n_mtbs_per_modis[n_mtbs_per_modis$n > 1,])
big_table[1, 8] <- mean(n_modis_per_mtbs$n)
big_table[1, 9] <- median(n_modis_per_mtbs$n)

max1 <- max(n_modis_per_mtbs$n)

big_table[1, 10] <- max1

big_table[1, 11] <- paste(n_modis_per_mtbs[n_modis_per_mtbs$n == max1,]$Var1, collapse = " ")
big_table[1, 12] <- mean(n_mtbs_per_modis$n)
big_table[1, 13] <- median(n_mtbs_per_modis$n)

max2 <- max(n_mtbs_per_modis$n)

big_table[1,14] <- max2

which2 <- as.numeric(n_mtbs_per_modis[n_mtbs_per_modis$n == max2,]$Var1)

big_table[1,15] <- paste(as.character(which2), collapse = " ")
big_table[1,16] <- NN==nrow(long_mt_mo)
big_table[1,17] <- length(unique(long_mt_mo$modis_id))
big_table[1,18] <- sum(results$n)
big_table[1,19] <- SS
big_table[1,20] <- TT
big_table[1,21] <- paste(as.character(dplyr::filter(long_mt_mo, modis_id == first(which2))$mtbs_cast_id),collapse = " ")


write.csv(big_table, paste0("data/",bt_fn))
