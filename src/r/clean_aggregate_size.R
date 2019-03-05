# Extract fire size 
if (!file.exists(file.path(stat_out, 'area_km2_pts_all.rds'))) {
  event_list <- list.files(yearly_events, pattern = 'events.tif', recursive = TRUE, full.names = TRUE)
  
  events_raw <- raster::stack(event_list) 
  events_rst <- events_raw %>%
    raster::reclassify(., cbind(-Inf, 1, NA), right = FALSE)
  names(events_rst) <- names(events_raw)
  
  for (i in 1:nlayers(events_rst)) {
    # Split out the year value from the file name
    name <- names(events_rst[[i]]) %>%
      strsplit(split = "_") %>%
      unlist
    name <- name[3]
    print(name)
    
    res <- events_rst[[i]] %>%
      rasterToPoints(., spatial = TRUE) %>%  # convert the raster to points.  
      st_as_sf() %>%
      gather(var, fire_id, -geometry) %>%
      separate(var, into = c("v1", 'v2', 'year', 'v3'),
               sep = "_") %>% # convert wide to long and break out the year from the name
      dplyr::select(year, fire_id, geometry) %>%
      dplyr::mutate(fire_id = paste0(fire_id, '_', year)) %>%
      dplyr::mutate(fire_id = group_indices(., fire_id)) 
    
    st_write(res, file.path(stat_out, paste0('full_events_pts_all_', name, '.gpkg')),
             delete_layer = TRUE)  
    
    res <- res %>%
      dplyr::group_by(fire_id) %>%
      dplyr::summarise(pixel_count = n(),
                       year = first(year)) %>%
      dplyr::mutate(area_km2 = (pixel_count*xres(events_rst[[i]])*yres(events_rst[[i]]))/1000000) %>%
      sf::st_cast('POINT') 
    
    sf::st_write(res, file.path(stat_out, paste0('area_km2_pts_all_', name, '.gpkg')),
                 delete_layer = TRUE)     
  }
  
  shp_list <- list.files(stat_out, pattern = 'area_km2_pts_all_20', recursive = TRUE, full.names = TRUE)
  
  area_km2_pts <- lapply(shp_list, FUN = function(x) { imported <- st_read(x) })
  
  #bind all of these together in one dataframe
  area_km2_pts <- do.call(rbind, area_km2_pts)
  
  area_km2_ecoreg_pts <- area_km2_pts %>%
    st_join(., ecoregions_l4321) %>%
    mutate(fire_id = row_number()) %>%
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., geom) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
  
  write_rds(area_km2_ecoreg_pts, file.path(stat_out, 'area_km2_pts_all.rds'))
} else {
  area_km2_ecoreg_pts <- st_read(file.path(stat_out, 'area_km2_pts_all.rds'))
}


if (!file.exists(file.path(stat_out, 'lvl1_eco_area_km2_ts.rds'))) {
  lvl1_eco_area_km2_ts <- aggregate_stats(area_km2_ecoreg_pts, na_l1name, area_km2)
  write_rds(lvl1_eco_area_km2_ts, file.path(stat_out, 'lvl1_eco_area_km2_ts.rds'))
  
  } else {
    lvl1_eco_area_km2_ts <- read_rds(file.path(stat_out, 'lvl1_eco_area_km2_ts.rds'))
  }

if (!file.exists(file.path(stat_out, 'lvl3_eco_area_km2_ts.rds'))) {
  lvl3_eco_area_km2_ts <- aggregate_stats(us_l3name, na_l1name, area_km2)
  write_rds(lvl3_eco_area_km2_ts, file.path(stat_out, 'lvl3_eco_area_km2_ts.rds'))
  
} else {
  lvl3_eco_area_km2_ts <- read_rds(file.path(stat_out, 'lvl3_eco_area_km2_ts.rds'))
}