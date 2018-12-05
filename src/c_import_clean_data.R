
# Download and import the Level 4 Ecoregions data, which has Levels 4, 3, 2, 1
# Download will only happen once as long as the file exists
if (!exists("ecoregions_l4321")){
  if(!file.exists(file.path(ecoregionl4_prefix, 'us_eco_l4_no_st.shp'))) {
    # Download ecoregion level 4
    ecol4_shp <- file.path(ecoregionl4_prefix, 'us_eco_l4_no_st.shp')
    download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
                  ecoregionl4_prefix,
                  ecol4_shp,
                  'us_eco_l4_no_st',
                  ecoregionl4_prefix)
    }
  
  ecoregions_l4321 <- st_read(file.path(ecoregionl4_prefix, 'us_eco_l4_no_st.shp')) %>%
    sf::st_transform(st_crs(usa)) %>%
    st_make_valid() %>%
    group_by(US_L4NAME, US_L3NAME, NA_L2NAME, NA_L1NAME) %>%
    summarise() %>%
    sf::st_simplify(., preserveTopology = TRUE, dTolerance = 100) %>%
    mutate(region = if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                       "TROPICAL WET FORESTS",
                                                       "NORTHERN FORESTS"), "East",
                                      if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                               "SOUTHERN SEMI-ARID HIGHLANDS",
                                                               "TEMPERATE SIERRAS",
                                                               "MEDITERRANEAN CALIFORNIA",
                                                               "NORTHWESTERN FORESTED MOUNTAINS",
                                                               "MARINE WEST COAST FOREST"), "West", "Central"))) %>%
    setNames(tolower(names(.)))
  
  ecoregions_l4321 %>%
    st_write(., file.path(ecoregion_out, 'us_eco_l4321.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
} else {
  ecoregions_l4321 <- st_read(file.path(ecoregion_out, 'us_eco_l4321.gpkg'))
  }

# Import and clean the MTBS polygons
if (!exists('mtbs_fire')) {
  
  mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
  if (!file.exists(mtbs_shp)) {
    file.download(file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp'),
                  mtbs_prefix, "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip")
    
  }
  
  mtbs_fire <- st_read(dsn = file.path(mtbs_prefix, 'mtbs_perimeter_data_v2'),
                       layer = 'dissolve_mtbs_perims_1984-2015_DD_20170501', quiet= TRUE) %>%
    filter(Year >= '2001') %>%
    st_transform(p4string_ea) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(usa)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy, acres) %>%
    # Below we are categorizing the fires as in the East or West based on the -97th parallel - which is what MTBS uses
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., st_centroid(geometry)) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(p4string_ea)
}

if(!file.exists(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))) {
  mtbs_ecoreg <- mtbs_fire %>%
    st_intersection(., ecoregions_l4321) %>%
    mutate(mtbs_ba_ha = acres*0.404686,
           mtbs_ba_ecoreg_ha = as.numeric(st_area(geometry))*0.0001) 
  
  mtbs_ecoreg %>%
    st_write(., file.path(fire_dir, 'mtbs_ecoreg.gpkg'), delete_layer = TRUE)
  
  system(paste0('aws s3 sync data' , ' ', s3_base))
  
} else {
  mtbs_ecoreg <- st_read(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))
}

# Extract fire size for level 4 ecoregions - this will allow us to join in level 3,2,1 ecoregions
if (!exists('lvl1_eco_area_km2_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_area_km2_all.rds'))) {
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
        mutate(fire_id = row_number())
      
      write_rds(area_km2_ecoreg_pts, file.path(stat_out, 'area_km2_pts_all.rds'))
    } else {
      area_km2_ecoreg_pts <- st_read(file.path(stat_out, 'area_km2_pts_all.rds'))
    }
    
    lvl4_eco_area_km2_ts <- area_km2_ecoreg_pts %>%
      group_by(us_l4name, year) %>%
      summarise(n_fire_events = n(),
                sum_area_km2 = sum(area_km2, na.rm = TRUE),
                min_area_km2 = min(area_km2, na.rm = TRUE),
                max_area_km2 = max(area_km2, na.rm = TRUE),
                mean_area_km2 = mean(area_km2, na.rm = TRUE),
                meadian_area_km2 = median(area_km2, na.rm = TRUE),
                sd_area_km2 = mean(area_km2, na.rm = TRUE))  %>%
      mutate(se_area_km2 = sd_area_km2 / sqrt(n_fire_events),
             lower_95ci_area_km2 = mean_area_km2 - qt(1 - (0.05 / 2), n_fire_events - 1) * se_area_km2,
             upper_95ci_area_km2 = mean_area_km2 + qt(1 - (0.05 / 2), n_fire_events - 1) * se_area_km2)
    
    write_rds(lvl4_eco_area_km2_ts, file.path(stat_out, 'lvl4_eco_area_km2_ts.rds'))
    
    lvl4_eco_area_km2_slim <- area_km2_ecoreg_pts %>%
      group_by(us_l4name) %>%
      summarise(n_fire_events = n(),
                sum_area_km2 = sum(area_km2, na.rm = TRUE),
                min_area_km2 = min(area_km2, na.rm = TRUE),
                max_area_km2 = max(area_km2, na.rm = TRUE),
                mean_area_km2 = mean(area_km2, na.rm = TRUE),
                meadian_area_km2 = median(area_km2, na.rm = TRUE),
                sd_area_km2 = mean(area_km2, na.rm = TRUE))  %>%
      mutate(se_area_km2 = sd_area_km2 / sqrt(n_fire_events),
             lower_95ci_area_km2 = mean_area_km2 - qt(1 - (0.05 / 2), n_fire_events - 1) * se_area_km2,
             upper_95ci_area_km2 = mean_area_km2 + qt(1 - (0.05 / 2), n_fire_events - 1) * se_area_km2)
    
    write_rds(lvl4_eco_area_km2_slim, file.path(stat_out, 'lvl4_eco_area_km2_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  lvl1_eco_area_km2_ts <- read_rds(file.path(stat_out, 'lvl1_eco_area_km2_ts.rds'))
  lvl1_eco_area_km2_slim <- read_rds(file.path(stat_out, 'lvl1_eco_area_km2_slim.rds'))
  mtbs_modis_area_comp <- st_read(file.path(stat_out, 'mtbs_modis_area_comp.gpkg'))
}

# Extract fire spread rate for level 1 ecoregions
if (!exists('lvl1_eco_fsr_ts')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_fsr_all.rds'))) {
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

    lvl1_mean_ci <- fsr_ecoreg_pts %>%
      group_by(na_l1name) %>%
      do(data.frame(rbind(smean.cl.boot(.$fsr, B = 10000)))) 
    
    lvl1_eco_fsr_ts <- fsr_ecoreg_pts %>%
      group_by(na_l1name, year) %>%
      summarise(n_fire_events = n(),
                sum_fsr = sum(fsr, na.rm = TRUE),
                min_fsr = min(fsr, na.rm = TRUE),
                max_fsr = max(fsr, na.rm = TRUE),
                mean_fsr = mean(fsr, na.rm = TRUE),
                meadian_fsr = median(fsr, na.rm = TRUE),
                sd_fsr = mean(fsr, na.rm = TRUE))  %>%
      mutate(se_fsr = sd_fsr / sqrt(n_fire_events),
             lower_95ci_fsr = mean_fsr - qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr,
             upper_95ci_fsr = mean_fsr + qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr)
    
    write_rds(lvl1_eco_fsr_ts, file.path(stat_out, 'lvl1_eco_fsr_ts.rds'))
    
    lvl1_eco_fsr_slim <- fsr_ecoreg_pts %>%
      group_by(na_l1name) %>%
      summarise(n_fire_events = n(),
                sum_fsr = sum(fsr, na.rm = TRUE),
                min_fsr = min(fsr, na.rm = TRUE),
                max_fsr = max(fsr, na.rm = TRUE),
                mean_fsr = mean(fsr, na.rm = TRUE),
                meadian_fsr = median(fsr, na.rm = TRUE),
                sd_fsr = mean(fsr, na.rm = TRUE))  %>%
      mutate(se_fsr = sd_fsr / sqrt(n_fire_events),
             lower_95ci_fsr = mean_fsr - qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr,
             upper_95ci_fsr = mean_fsr + qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr)
    
    write_rds(lvl1_eco_fsr_slim, file.path(stat_out, 'lvl1_eco_fsr_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
    if (!file.exists(file.path(stat_out, 'lvl4321_intersect_pts.gpkg'))) {
      lvl4321_eco_fsr <- fsr_ecoreg_pts %>%
        dplyr::select(-na_l1name) %>%
        st_intersection(. , ecoregions_l4321) 
      write_rds(lvl4321_eco_fsr, file.path(stat_out, 'lvl4321_intersect_pts.gpkg'))
      
    } else {
      lvl4321_eco_fsr <- st_read(file.path(stat_out, 'lvl4321_intersect_pts.gpkg'))
    }
    
    lvl3_mean_ci <- lvl4321_eco_fsr %>%
      group_by(us_l3name) %>%
      do(data.frame(rbind(smean.cl.boot(.$fsr, B = 10000)))) 
    
    lvl3_eco_fsr_ts <- lvl4321_eco_fsr %>%
      group_by(us_l3name, year) %>%
      summarise(n_fire_events = n(),
                sum_fsr = sum(fsr, na.rm = TRUE),
                min_fsr = min(fsr, na.rm = TRUE),
                max_fsr = max(fsr, na.rm = TRUE),
                mean_fsr = mean(fsr, na.rm = TRUE),
                meadian_fsr = median(fsr, na.rm = TRUE),
                sd_fsr = mean(fsr, na.rm = TRUE))  %>%
      mutate(se_fsr = sd_fsr / sqrt(n_fire_events),
             lower_95ci_fsr = mean_fsr - qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr,
             upper_95ci_fsr = mean_fsr + qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr)
    
    write_rds(lvl3_eco_fsr_ts, file.path(stat_out, 'lvl3_eco_fsr_ts.rds'))
    
    lvl3_eco_fsr_slim <- lvl4321_eco_fsr %>%
      group_by(us_l3name) %>%
      summarise(n_fire_events = n(),
                sum_fsr = sum(fsr, na.rm = TRUE),
                min_fsr = min(fsr, na.rm = TRUE),
                max_fsr = max(fsr, na.rm = TRUE),
                mean_fsr = mean(fsr, na.rm = TRUE),
                meadian_fsr = median(fsr, na.rm = TRUE),
                sd_fsr = mean(fsr, na.rm = TRUE))  %>%
      mutate(se_fsr = sd_fsr / sqrt(n_fire_events),
             lower_95ci_fsr = mean_fsr - qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr,
             upper_95ci_fsr = mean_fsr + qt(1 - (0.05 / 2), n_fire_events - 1) * se_fsr)
    
    write_rds(lvl3_eco_fsr_slim, file.path(stat_out, 'lvl3_eco_fsr_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  }
} else {
  fsr_ecoreg_pts <- read_rds(file.path(stat_out, 'fsr_pts_all.rds'))
  lvl1_eco_fsr_ts <- read_rds(file.path(stat_out, 'lvl1_eco_fsr_ts.rds'))
  lvl1_eco_fsr_slim <- read_rds(file.path(stat_out, 'lvl1_eco_fsr_slim.rds'))
  lvl4_eco_fsr_ts <- read_rds(file.path(stat_out, 'lvl4_eco_fsr_ts.rds'))
  lvl4_eco_fsr_slim <- read_rds(file.path(stat_out, 'lvl4_eco_fsr_slim.rds'))
}

# Extract fire duration for level 1 ecoregions
if (!exists('duration_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_duration_all.rds'))) {
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
        st_join(., ecoregions_l4321)
      
      write_rds(duration_ecoreg_pts, file.path(stat_out, 'duration_pts_all.rds'))
      
    } else {
      duration_ecoreg_pts <- read_rds(file.path(stat_out, 'duration_pts_all.rds'))
    }
    
    lvl1_eco_duration_ts <- duration_ecoreg_pts %>%
      group_by(na_l1name, year) %>%
      summarise(n_fire_events = n(),
                sum_duration = sum(duration, na.rm = TRUE),
                min_duration = min(duration, na.rm = TRUE),
                max_duration = max(duration, na.rm = TRUE),
                mean_duration = mean(duration, na.rm = TRUE),
                meadian_duration = median(duration, na.rm = TRUE),
                sd_duration = mean(duration, na.rm = TRUE))  %>%
      mutate(se_duration = sd_duration / sqrt(n_fire_events),
             lower_95ci_duration = mean_duration - qt(1 - (0.05 / 2), n_fire_events - 1) * se_duration,
             upper_95ci_duration = mean_duration + qt(1 - (0.05 / 2), n_fire_events - 1) * se_duration)
    
    write_rds(lvl1_eco_duration_ts, file.path(stat_out, 'lvl1_eco_duration_ts.rds'))
    
    lvl1_eco_duration_slim <- duration_ecoreg_pts %>%
      group_by(na_l1name) %>%
      summarise(n_fire_events = n(),
                sum_duration = sum(duration, na.rm = TRUE),
                min_duration = min(duration, na.rm = TRUE),
                max_duration = max(duration, na.rm = TRUE),
                mean_duration = mean(duration, na.rm = TRUE),
                meadian_duration = median(duration, na.rm = TRUE),
                sd_duration = mean(duration, na.rm = TRUE))  %>%
      mutate(se_duration = sd_duration / sqrt(n_fire_events),
             lower_95ci_duration = mean_duration - qt(1 - (0.05 / 2), n_fire_events - 1) * se_duration,
             upper_95ci_duration = mean_duration + qt(1 - (0.05 / 2), n_fire_events - 1) * se_duration)
    
    write_rds(lvl1_eco_duration_slim, file.path(stat_out, 'lvl1_eco_duration_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  lvl1_eco_duration_ts <- read_rds(file.path(stat_out, 'lvl1_eco_duration_ts.rds'))
  lvl1_eco_duration_slim <- read_rds(file.path(stat_out, 'lvl1_eco_duration_slim.rds'))
}

fsr_vs <- fsr_ecoreg_pts %>%
  left_join(., as.data.frame(area_km2_ecoreg_pts) %>% dplyr::select(-geom), by = c('fire_id', 'us_l4name', 'us_l3name', 'na_l2name', 'na_l1name', 'region', 'year')) %>%
  left_join(., as.data.frame(duration_ecoreg_pts) %>% dplyr::select(-geom), by = c('fire_id', 'us_l4name', 'us_l3name', 'na_l2name', 'na_l1name', 'region', 'year')) %>%
  left_join(., as.data.frame(ungroup(ecoregions_l4321)) %>% dplyr::select(-geometry), by = c('us_l4name', 'us_l3name', 'na_l2name', 'na_l1name', 'region'))
  

# Extract first burn date for level 1 ecoregions
if (!exists('lvl1_eco_firstbd_ts')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_firstbd_all.rds'))) {
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
        st_join(., ecoreg_slim)
      
      st_write(firstbd_ecoreg_pts, file.path(stat_out, 'firstbd_pts_all.gpkg'),
               delete_layer = TRUE)
      
    } else {
      firstbd_ecoreg_pts <- st_read(file.path(stat_out, 'firstbd_pts_all.gpkg'))
    }
    
    lvl1_eco_firstbd_ts <- firstbd_ecoreg_pts %>%
      group_by(na_l1name, year) %>%
      summarise(n_fire_events = n(),
                sum_firstbd = sum(firstbd, na.rm = TRUE),
                min_firstbd = min(firstbd, na.rm = TRUE),
                max_firstbd = max(firstbd, na.rm = TRUE),
                mean_firstbd = mean(firstbd, na.rm = TRUE),
                meadian_firstbd = median(firstbd, na.rm = TRUE),
                sd_firstbd = mean(firstbd, na.rm = TRUE))  %>%
      mutate(se_firstbd = sd_firstbd / sqrt(n_fire_events),
             lower_95ci_firstbd = mean_firstbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_firstbd,
             upper_95ci_firstbd = mean_firstbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_firstbd)
    
    write_rds(lvl1_eco_firstbd_ts, file.path(stat_out, 'lvl1_eco_firstbd_ts.rds'))
    
    lvl1_eco_firstbd_slim <- firstbd_ecoreg_pts %>%
      group_by(na_l1name) %>%
      summarise(n_fire_events = n(),
                sum_firstbd = sum(firstbd, na.rm = TRUE),
                min_firstbd = min(firstbd, na.rm = TRUE),
                max_firstbd = max(firstbd, na.rm = TRUE),
                mean_firstbd = mean(firstbd, na.rm = TRUE),
                meadian_firstbd = median(firstbd, na.rm = TRUE),
                sd_firstbd = mean(firstbd, na.rm = TRUE))  %>%
      mutate(se_firstbd = sd_firstbd / sqrt(n_fire_events),
             lower_95ci_firstbd = mean_firstbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_firstbd,
             upper_95ci_firstbd = mean_firstbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_firstbd)
    
    write_rds(lvl1_eco_firstbd_slim, file.path(stat_out, 'lvl1_eco_firstbd_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  lvl1_eco_firstbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_firstbd_ts.rds'))
  lvl1_eco_firstbd_slim <- read_rds(file.path(stat_out, 'lvl1_eco_firstbd_slim.rds'))
}

# Extract last burn date size for level 1 ecoregions
if (!exists('lvl1_eco_lastbd_ts')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_lastbd_all.rds'))) {
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
        st_join(., ecoreg_slim)
      
      st_write(lastbd_ecoreg_pts, file.path(stat_out, 'lastbd_pts_all.gpkg'),
               delete_layer = TRUE)
      
    } else {
      lastbd_ecoreg_pts <- st_read(file.path(stat_out, 'lastbd_pts_all.gpkg'))
    }
    
    lvl1_eco_lastbd_ts <- lastbd_ecoreg_pts %>%
      group_by(na_l1name, year) %>%
      summarise(n_fire_events = n(),
                sum_lastbd = sum(lastbd, na.rm = TRUE),
                min_lastbd = min(lastbd, na.rm = TRUE),
                max_lastbd = max(lastbd, na.rm = TRUE),
                mean_lastbd = mean(lastbd, na.rm = TRUE),
                meadian_lastbd = median(lastbd, na.rm = TRUE),
                sd_lastbd = mean(lastbd, na.rm = TRUE))  %>%
      mutate(se_lastbd = sd_lastbd / sqrt(n_fire_events),
             lower_95ci_lastbd = mean_lastbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_lastbd,
             upper_95ci_lastbd = mean_lastbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_lastbd)
    
    write_rds(lvl1_eco_lastbd_ts, file.path(stat_out, 'lvl1_eco_lastbd_ts.rds'))
    
    lvl1_eco_lastbd_slim <- lastbd_ecoreg_pts %>%
      group_by(na_l1name) %>%
      summarise(n_fire_events = n(),
                sum_lastbd = sum(lastbd, na.rm = TRUE),
                min_lastbd = min(lastbd, na.rm = TRUE),
                max_lastbd = max(lastbd, na.rm = TRUE),
                mean_lastbd = mean(lastbd, na.rm = TRUE),
                meadian_lastbd = median(lastbd, na.rm = TRUE),
                sd_lastbd = mean(lastbd, na.rm = TRUE))  %>%
      mutate(se_lastbd = sd_lastbd / sqrt(n_fire_events),
             lower_95ci_lastbd = mean_lastbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_lastbd,
             upper_95ci_lastbd = mean_lastbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_lastbd)
    
    write_rds(lvl1_eco_lastbd_slim, file.path(stat_out, 'lvl1_eco_lastbd_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  lvl1_eco_lastbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_lastbd_ts.rds'))
  lvl1_eco_lastbd_slim <- read_rds(file.path(stat_out, 'lvl1_eco_lastbd_slim.rds'))
}

# Extract peak burn date size for level 1 ecoregions
if (!exists('lvl1_eco_maxbd_ts')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_maxbd_all.rds'))) {
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
        st_join(., ecoreg_slim)
      
      st_write(maxbd_ecoreg_pts, file.path(stat_out, 'maxbd_pts_all.gpkg'),
               delete_layer = TRUE)
      
    } else {
      maxbd_ecoreg_pts <- st_read(file.path(stat_out, 'maxbd_pts_all.gpkg'))
    }
    
    lvl1_eco_maxbd_ts <- maxbd_ecoreg_pts %>%
      group_by(na_l1name, year) %>%
      summarise(n_fire_events = n(),
                sum_maxbd = sum(maxbd, na.rm = TRUE),
                min_maxbd = min(maxbd, na.rm = TRUE),
                max_maxbd = max(maxbd, na.rm = TRUE),
                mean_maxbd = mean(maxbd, na.rm = TRUE),
                meadian_maxbd = median(maxbd, na.rm = TRUE),
                sd_maxbd = mean(maxbd, na.rm = TRUE))  %>%
      mutate(se_maxbd = sd_maxbd / sqrt(n_fire_events),
             lower_95ci_maxbd = mean_maxbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_maxbd,
             upper_95ci_maxbd = mean_maxbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_maxbd)
    
    write_rds(lvl1_eco_maxbd_ts, file.path(stat_out, 'lvl1_eco_maxbd_ts.rds'))
    
    lvl1_eco_maxbd_slim <- maxbd_ecoreg_pts %>%
      group_by(na_l1name) %>%
      summarise(n_fire_events = n(),
                sum_maxbd = sum(maxbd, na.rm = TRUE),
                min_maxbd = min(maxbd, na.rm = TRUE),
                max_maxbd = max(maxbd, na.rm = TRUE),
                mean_maxbd = mean(maxbd, na.rm = TRUE),
                meadian_maxbd = median(maxbd, na.rm = TRUE),
                sd_maxbd = mean(maxbd, na.rm = TRUE))  %>%
      mutate(se_maxbd = sd_maxbd / sqrt(n_fire_events),
             lower_95ci_maxbd = mean_maxbd - qt(1 - (0.05 / 2), n_fire_events - 1) * se_maxbd,
             upper_95ci_maxbd = mean_maxbd + qt(1 - (0.05 / 2), n_fire_events - 1) * se_maxbd)
    
    write_rds(lvl1_eco_maxbd_slim, file.path(stat_out, 'lvl1_eco_maxbd_slim.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
  }
} else {
  lvl1_eco_maxbd_ts <- read_rds(file.path(stat_out, 'lvl1_eco_maxbd_ts.rds'))
  lvl1_eco_maxbd_slim <- read_rds(file.path(stat_out, 'lvl1_eco_maxbd_slim.rds'))
}

