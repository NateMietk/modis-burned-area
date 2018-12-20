# Extract first burn date for level 1 ecoregions
if (!file.exists(file.path(stat_out, 'firstbd_pts_all.gpkg'))) {
  firstbd_list <- list.files(yearly_events, pattern = 'firstbd.tif', recursive = TRUE, full.names = TRUE)
  
  firstbd_raw <- raster::stack(firstbd_list) 
  firstbd_rst <- firstbd_raw %>%
    raster::reclassify(., cbind(-Inf, 0.01, NA), right = FALSE)
  names(firstbd_rst) <- names(firstbd_raw)
  
  for (i in 1:nlayers(firstbd_rst)) {
    name <- names(firstbd_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    shp_mask <- st_read(file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')))
    
    res <- firstbd_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf() %>%
      gather(var, firstbd, -geometry) %>%
      dplyr::select(firstbd, geometry) 
    res <- shp_mask %>%
      st_join(., res) %>%
      distinct(fire_id, .keep_all = TRUE)
    
    st_write(res, file.path(stat_out, paste0('firstbd_pts_', name, '.gpkg')),
             delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'firstbd_pts_20', recursive = TRUE, full.names = TRUE)
  
  firstbd_pts <- lapply(shp_list,
                        FUN = function(x) {
                          input_name <- x
                          imported <- st_read(x)
                        })
  
  #bind all of these together in one dataframe
  firstbd_pts <- do.call(rbind, firstbd_pts)
  
  firstbd_ecoreg_pts <- firstbd_pts %>%
    st_join(., ecoregions_l4321) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
  
  st_write(firstbd_ecoreg_pts, file.path(stat_out, 'firstbd_pts_all.gpkg'),
           delete_layer = TRUE)
  
} else {
  firstbd_ecoreg_pts <- st_read(file.path(stat_out, 'firstbd_pts_all.gpkg'))
}

    
# Aggregate the firstbd events by Level 1 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl1_eco_firstbd_ts.rds'))) {
  lvl1_eco_firstbd_ts <- aggregate_stats(firstbd_pts, na_l1name, fsr)
  
  write_rds(lvl1_eco_firstbd_ts, file.path(stat_out, 'lvl1_eco_firstbd_ts.rds'))
} else {
  lvl1_eco_firstbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_firstbd_ts.rds'))
}

# Aggregate the firstbd events by Level 3 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl3_eco_firstbd_ts.rds'))) {    
  lvl3_eco_firstbd_ts <- aggregate_stats(firstbd_pts, us_l3name, fsr)
  
  write_rds(lvl3_eco_firstbd_ts, file.path(stat_out, 'lvl3_eco_firstbd_ts.rds'))
} else {
  lvl3_eco_firstbd_ts <- read_rds(file.path(stat_out, 'lvl3_eco_firstbd_ts.rds'))
}
