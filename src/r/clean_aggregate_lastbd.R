# Extract last burn date size for level 1 ecoregions
if (!file.exists(file.path(stat_out, 'lastbd_pts_all.gpkg'))) {
  lastbd_list <- list.files(yearly_events, pattern = 'lastbd.tif', recursive = TRUE, full.names = TRUE)
  
  lastbd_raw <- raster::stack(lastbd_list) 
  lastbd_rst <- lastbd_raw %>%
    raster::reclassify(., cbind(-Inf, 0.01, NA), right = FALSE)
  names(lastbd_rst) <- names(lastbd_raw)
  
  for (i in 1:nlayers(lastbd_rst)) {
    name <- names(lastbd_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    shp_mask <- st_read(file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')))
    
    res <- lastbd_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf() %>%
      gather(var, lastbd, -geometry) %>%
      dplyr::select(lastbd, geometry) 
    res <- shp_mask %>%
      st_join(., res) %>%
      distinct(fire_id, .keep_all = TRUE)
    
    st_write(res, file.path(stat_out, paste0('lastbd_pts_', name, '.gpkg')),
             delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'lastbd_pts_20', recursive = TRUE, full.names = TRUE)
  
  lastbd_pts <- lapply(shp_list,
                       FUN = function(x) {
                         input_name <- x
                         imported <- st_read(x)
                       })
  
  #bind all of these together in one dataframe
  lastbd_pts <- do.call(rbind, lastbd_pts)
  
  lastbd_ecoreg_pts <- lastbd_pts %>%
    st_join(., ecoregions_l4321) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)  
  st_write(lastbd_ecoreg_pts, file.path(stat_out, 'lastbd_pts_all.gpkg'),
           delete_layer = TRUE)
  
} else {
  lastbd_ecoreg_pts <- st_read(file.path(stat_out, 'lastbd_pts_all.gpkg'))
}


# Aggregate the lastbd events by Level 1 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl1_eco_lastbd_ts.rds'))) {
  lvl1_eco_lastbd_ts <- aggregate_stats(lastbd_pts, na_l1name, lastbd)
  
  write_rds(lvl1_eco_lastbd_ts, file.path(stat_out, 'lvl1_eco_lastbd_ts.rds'))
} else {
  lvl1_eco_lastbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_lastbd_ts.rds'))
}

# Aggregate the lastbd events by Level 3 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl3_eco_lastbd_ts.rds'))) {    
  lvl3_eco_lastbd_ts <- aggregate_stats(lastbd_pts, us_l3name, lastbd)
  
  write_rds(lvl3_eco_lastbd_ts, file.path(stat_out, 'lvl3_eco_lastbd_ts.rds'))
} else {
  lvl3_eco_lastbd_ts <- read_rds(file.path(stat_out, 'lvl3_eco_lastbd_ts.rds'))
}
