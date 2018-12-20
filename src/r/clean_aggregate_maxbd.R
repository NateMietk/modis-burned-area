# Extract peak burn date size 
if (!file.exists(file.path(stat_out, 'maxbd_pts_all.gpkg'))) {
  maxbd_list <- list.files(yearly_events, pattern = 'maxbd.tif', recursive = TRUE, full.names = TRUE)
  
  maxbd_raw <- raster::stack(maxbd_list) 
  maxbd_rst <- maxbd_raw %>%
    raster::reclassify(., cbind(-Inf, 0.01, NA), right = FALSE)
  names(maxbd_rst) <- names(maxbd_raw)
  
  for (i in 1:nlayers(maxbd_rst)) {
    name <- names(maxbd_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    shp_mask <- st_read(file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')))
    
    res <- maxbd_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf() %>%
      gather(var, maxbd, -geometry) %>%
      dplyr::select(maxbd, geometry) 
    res <- shp_mask %>%
      st_join(., res) %>%
      distinct(fire_id, .keep_all = TRUE)
    
    st_write(res, file.path(stat_out, paste0('maxbd_pts_', name, '.gpkg')),
             delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'maxbd_pts_20', recursive = TRUE, full.names = TRUE)
  
  maxbd_pts <- lapply(shp_list,
                      FUN = function(x) {
                        input_name <- x
                        imported <- st_read(x)
                      })
  
  #bind all of these together in one dataframe
  maxbd_pts <- do.call(rbind, maxbd_pts)
  
  maxbd_ecoreg_pts <- maxbd_pts %>%
    st_join(., ecoregions_l4321) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)  
  st_write(maxbd_ecoreg_pts, file.path(stat_out, 'maxbd_pts_all.gpkg'),
           delete_layer = TRUE)
  
} else {
  maxbd_ecoreg_pts <- st_read(file.path(stat_out, 'maxbd_pts_all.gpkg'))
}
  
# Aggregate the maxbd events by Level 1 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl1_eco_maxbd_ts.rds'))) {
  lvl1_eco_maxbd_ts <- aggregate_stats(maxbd_pts, na_l1name, maxbd)
  
  write_rds(lvl1_eco_maxbd_ts, file.path(stat_out, 'lvl1_eco_maxbd_ts.rds'))
} else {
  lvl1_eco_maxbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_maxbd_ts.rds'))
}

# Aggregate the maxbd events by Level 3 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl3_eco_maxbd_ts.rds'))) {    
  lvl3_eco_maxbd_ts <- aggregate_stats(maxbd_pts, us_l3name, maxbd)
  
  write_rds(lvl3_eco_maxbd_ts, file.path(stat_out, 'lvl3_eco_maxbd_ts.rds'))
} else {
  lvl3_eco_maxbd_ts <- read_rds(file.path(stat_out, 'lvl3_eco_maxbd_ts.rds'))
}