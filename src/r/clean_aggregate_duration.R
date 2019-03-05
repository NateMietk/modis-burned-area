# Extract fire duration
if (!file.exists(file.path(stat_out, 'duration_pts_all.rds'))) {
  duration_list <- list.files(yearly_events, pattern = 'duration.tif', recursive = TRUE, full.names = TRUE)
  
  duration_raw <- raster::stack(duration_list) 
  duration_rst <- duration_raw %>%
    raster::reclassify(., cbind(-Inf, 0.01, NA), right = FALSE)
  names(duration_rst) <- names(duration_raw)
  
  for (i in 1:nlayers(duration_rst)) {
    name <- names(duration_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    shp_mask <- st_read(file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')))
    
    res <- duration_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf() %>%
      gather(var, duration, -geometry) %>%
      dplyr::select(duration, geometry) 
    res <- shp_mask %>%
      st_join(., res) %>%
      distinct(fire_id, .keep_all = TRUE)
    
    st_write(res, file.path(stat_out, paste0('duration_pts_', name, '.gpkg')),
             delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'duration_pts_20', recursive = TRUE, full.names = TRUE)
  
  duration_pts <- lapply(shp_list,
                         FUN = function(x) {
                           input_name <- x
                           imported <- st_read(x)
                         })
  
  #bind all of these together in one dataframe
  duration_pts <- do.call(rbind, duration_pts)
  
  duration_ecoreg_pts <- duration_pts %>%
    st_join(., ecoregions_l4321) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
  
  write_rds(duration_ecoreg_pts, file.path(stat_out, 'duration_pts_all.rds'))
  
} else {
  duration_ecoreg_pts <- read_rds(file.path(stat_out, 'duration_pts_all.rds'))
}


# Aggregate the Duration events by Level 1 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl1_eco_duration_ts.rds'))) {
  lvl1_eco_duration_ts <- aggregate_stats(duration_ecoreg_pts, na_l1name, duration)
  
  write_rds(lvl1_eco_duration_ts, file.path(stat_out, 'lvl1_eco_duration_ts.rds'))
  } else {
    lvl1_eco_duration_ts <- read_rds(file.path(stat_out, 'lvl1_eco_duration_ts.rds'))
  }

# Aggregate the Duration events by Level 3 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl3_eco_duration_ts.rds'))) {
  lvl3_eco_duration_ts <- aggregate_stats(duration_ecoreg_pts, us_l3name, duration)
  
  write_rds(lvl3_eco_duration_ts, file.path(stat_out, 'lvl3_eco_duration_ts.rds'))
} else {
  lvl3_eco_duration_ts <- read_rds(file.path(stat_out, 'lvl3_eco_duration_ts.rds'))
}