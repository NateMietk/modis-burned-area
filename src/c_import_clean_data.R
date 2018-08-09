

# Import and clean the Level 1 Ecoregions
if (!exists('ecoregl1')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l1.gpkg'))) {
    
    # Download the Level 1 Ecoregions
    ecoregion_shp <- file.path(ecoregion_out, "NA_CEC_Eco_Level1.shp")
    if (!file.exists(ecoregion_shp)) {
      file.download(file.path(ecoregion_out, "NA_CEC_Eco_Level1.shp"),
                    ecoregion_out, "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip")
    }
    
    ecoregl1 <- st_read(dsn = ecoregion_out, layer = "NA_CEC_Eco_Level1") %>%
      sf::st_simplify(., preserveTopology = TRUE, dTolerance = 1000) %>%
      st_transform(st_crs(usa)) %>%  # e.g. US National Atlas Equal Area 
      st_make_valid() %>%
      st_intersection(., st_union(usa)) %>%
      mutate(region = as.factor(if_else(NA_L1NAME %in% c("EASTERN TEMPERATE FORESTS",
                                                         "TROPICAL WET FORESTS",
                                                         "NORTHERN FORESTS"), "East",
                                        if_else(NA_L1NAME %in% c("NORTH AMERICAN DESERTS",
                                                                 "SOUTHERN SEMI-ARID HIGHLANDS",
                                                                 "TEMPERATE SIERRAS",
                                                                 "MEDITERRANEAN CALIFORNIA",
                                                                 "NORTHWESTERN FORESTED MOUNTAINS",
                                                                 "MARINE WEST COAST FOREST"), "West", "Central")))) %>%
      setNames(tolower(names(.))) 
    
    st_write(ecoregl1, file.path(ecoregion_out, 'us_eco_l1.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
    
  } else {
    ecoregl1 <- sf::st_read(file.path(ecoregion_out, 'us_eco_l1.gpkg'))
    
    ecoreg_slim <- ecoregl1 %>%
      group_by(na_l1name) %>%
      summarise() %>%
      st_cast('MULTIPOLYGON')
  }
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
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy, acres) 
}

if(!file.exists(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))) {
  mtbs_ecoreg <- mtbs_fire %>%
    st_intersection(., ecoregl1) %>%
    dplyr::select(-shape_leng, -shape_area) %>%
    mutate(mtbs_ba_ha = acres*0.404686,
           mtbs_ba_ecoreg_ha = as.numeric(st_area(geometry))*0.0001) %>%
    filter(na_l1name != 'WATER')
  
  mtbs_ecoreg %>% 
    st_write(., file.path(fire_dir, 'mtbs_ecoreg.gpkg'))
  
  system(paste0('aws s3 sync data' , ' ', s3_base)) 
  
} else {
  mtbs_ecoreg <- st_read(file.path(fire_dir, 'mtbs_ecoreg.gpkg'))
}

as.data.frame(mtbs_ecoreg) %>%
  group_by(na_l1name) %>%
  summarise(fire_freq = n(),
            burned_area = sum(mtbs_ba_ecoreg_ha))

clean_velox_extract <- function(df, shapefile, var) {
  res <- df %>%
    mutate(na_l1name = as.data.frame(shapefile)$na_l1name) %>%
    dplyr::select(-ID_sp) %>%
    gather(key = key, value = var , -na_l1name) %>%
    separate(key,
             into = c("v1", 'v2', 'year', 'v3'),
             sep = "_") %>%
    mutate(na_l1name = as.factor(na_l1name),
           year = as.integer(year)) %>%
    dplyr::select(-v1, -v2, -v3) 
  return(res)
}

if (!exists('fsr_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))) {
    event_list <- list.files(yearly_events, pattern = 'events.tif', recursive = TRUE, full.names = TRUE)
    
    fire_events <- raster::stack(event_list) %>%
      raster::reclassify(., cbind(-Inf, 1, NA), right = FALSE)
    fire_events_velox <- velox(fire_events)
    
    fire_event_count <- fire_events_velox$extract(sp = ecoreg_slim, fun = function(x) length(unique(x)), 
                                                  small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(fire_event_count) <- c('ID_sp', names(fire_events))
    fire_event_count_df <- clean_velox_extract(fire_event_count, ecoreg_slim, 'n_fire_events')
    
    write_rds(fire_event_count_df, file.path(stat_out, 'n_fire_events.rds'))
    system(paste0("aws s3 sync ", prefix, " ", s3_base))
    
  } else {
    fire_event_count_df <- read_rds(file.path(stat_out, 'n_fire_events.rds'))
  }
}

# Extract mean fire rate of spread for level 1 ecoregions
if (!exists('fsr_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))) {
    
    fsr_list <- list.files(yearly_events, pattern = 'fsr.tif', recursive = TRUE, full.names = TRUE)
    
    fsr <- raster::stack(fsr_list)
    fsr_velox <- velox(raster::reclassify(fsr, cbind(-Inf, 1, NA), right = FALSE))
    
    # Extract mean fire rate of spread
    fsr_extract_mean <- fsr_velox$extract(sp = ecoreg_slim, fun = function(x) mean(x, na.rm = TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(fsr_extract_mean) <- c('ID_sp', names(fsr))
    fsr_mean <- clean_velox_extract(fsr_extract_mean, ecoreg_slim, 'mean_spread_rate')
    
    # Extract median fire rate of spred
    fsr_extract_median <- fsr_velox$extract(sp = ecoreg_slim, fun = function(x) mean(x, na.rm = TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(fsr_extract_median) <- c('ID_sp', names(fsr))
    fsr_max <- clean_velox_extract(fsr_extract_median, ecoreg_slim, 'mean_spread_rate')
    
    # Extract SD of fire rate of spread
    fsr_extract_sd <- fsr_velox$extract(sp = ecoreg_slim, fun = function(x) sd(x, na.rm = TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(fsr_extract_sd) <- c('ID_sp', names(fsr))
    fsr_max <- clean_velox_extract(fsr_extract_sd, ecoreg_slim, 'mean_spread_rate')
    
    write_rds(fsr_df, file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))
    # system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))
    
  } else {
    fsr_df <- read_rds(file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))
  }
}








# Extract mean fire event duration for level 1 ecoregions
if (!exists('duration_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_duration_mean.rds'))) {
    duration_list <- list.files(yearly_events, pattern = 'duration.tif', recursive = TRUE, full.names = TRUE)
    
    duration <- raster::stack(duration_list) 
    duration_velox <- velox(raster::reclassify(duration, cbind(-Inf, 1, NA), right = FALSE))
    
    duration_df <- duration_velox$extract(sp = ecoreg_slim, fun = function(x) mean(x, na.rm = TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(duration_df) <- c('ID_sp', names(duration))
    duration_df <- duration_df %>%
      mutate(na_l1name = as.data.frame(ecoreg_slim)$na_l1name) %>%
      dplyr::select(-ID_sp) %>%
      gather(key = key, value = duration , -na_l1name) %>%
      separate(key,
               into = c("v1", 'v2', 'year', 'v3'),
               sep = "_") %>%
      mutate(na_l1name = as.factor(na_l1name),
             year = as.integer(year)) %>%
      dplyr::select(-v1, -v2, -v3) 
    
    write_rds(duration_df, file.path(stat_out, 'lvl1_eco_duration_mean.rds'))
    # system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))
    
  } else {
    duration_df <- read_rds(file.path(stat_out, 'lvl1_eco_duration_mean.rds'))
  }
}







# Extract mean fire rate of spread for level 1 ecoregions
if (!exists('fsr_df')) {
  if (!file.exists(file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))) {
    fsr_list <- list.files(yearly_events, pattern = 'fsr.tif', recursive = TRUE, full.names = TRUE)
    
    velox_slim <- velox(raster::stack(fsr_list[1]))
    
    fsr_df <- velox_slim$extract(sp = ecoreg_slim, fun = function(x) quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), small = TRUE, df = TRUE) %>%
      as_tibble()
    colnames(fsr_df) <- c('ID_sp', names(fsr))
    fsr_df <- fsr_df %>%
      mutate(na_l1name = as.data.frame(ecoreg_slim)$na_l1name) %>%
      dplyr::select(-ID_sp) %>%
      gather(key = key, value = spread_rate , -na_l1name) %>%
      separate(key,
               into = c("v1", 'v2', 'year', 'v3'),
               sep = "_") %>%
      mutate(na_l1name = as.factor(na_l1name),
             year = as.integer(year)) %>%
      dplyr::select(-v1, -v2, -v3) 
    
    write_rds(fsr_df, file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))
    # system(paste0("aws s3 sync ",  climate_dir, " ", s3_climate_prefix))
    
  } else {
    fsr_df <- read_rds(file.path(stat_out, 'lvl1_eco_fsr_mean.rds'))
  }
}


