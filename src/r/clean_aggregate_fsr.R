# Extract fire spread rate 
if (!file.exists(file.path(stat_out, 'fsr_pts_all.rds'))) {
  fsr_list <- list.files(yearly_events, pattern = 'fsr.tif', recursive = TRUE, full.names = TRUE)
  
  fsr_raw <- raster::stack(fsr_list) 
  fsr_rst <- fsr_raw %>%
    raster::reclassify(., cbind(-Inf, 0.01, NA), right = FALSE)
  names(fsr_rst) <- names(fsr_raw)
  
  for (i in 1:nlayers(fsr_rst)) {
    name <- names(fsr_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    shp_mask <- st_read(file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')))
    
    res <- fsr_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%
      st_as_sf() %>%
      gather(var, fsr, -geometry) %>%
      dplyr::select(fsr, geometry) 
    res <- shp_mask %>%
      st_join(., res) %>%
      distinct(fire_id, .keep_all = TRUE)
    
    st_write(res, file.path(stat_out, paste0('fsr_pts_', name, '.gpkg')),
             delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'fsr_pts_20', recursive = TRUE, full.names = TRUE)
  
  fsr_pts <- lapply(shp_list,
                    FUN = function(x) {
                      input_name <- x
                      imported <- st_read(x)
                    })
  
  #bind all of these together in one dataframe
  fsr_pts <- do.call(rbind, fsr_pts)
  
  fsr_ecoreg_pts <- fsr_pts %>%
    st_join(., ecoregions_l4321) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
  
  write_rds(fsr_ecoreg_pts, file.path(stat_out, 'fsr_pts_all.rds'))
  
} else {
  fsr_ecoreg_pts <- read_rds(file.path(stat_out, 'fsr_pts_all.rds'))
}


# Aggregate the FSR events by Level 1 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl1_eco_fsr_ts.rds'))) {
  lvl1_eco_fsr_ts <- aggregate_stats(fsr_pts, na_l1name, fsr)
  
  write_rds(lvl1_eco_fsr_ts, file.path(stat_out, 'lvl1_eco_fsr_ts.rds'))
  } else {
    lvl1_eco_fsr_ts <- read_rds(file.path(stat_out, 'lvl1_eco_fsr_ts.rds'))
  }

# Aggregate the FSR events by Level 3 Ecoregion
if (!file.exists(file.path(stat_out, 'lvl3_eco_fsr_ts.rds'))) {    
  lvl3_eco_fsr_ts <- aggregate_stats(fsr_pts, us_l3name, fsr)
  
  write_rds(lvl3_eco_fsr_ts, file.path(stat_out, 'lvl3_eco_fsr_ts.rds'))
  } else {
    lvl3_eco_fsr_ts <- read_rds(file.path(stat_out, 'lvl3_eco_fsr_ts.rds'))
  }


