# Download and import the Level 4 Ecoregions data, which has Levels 4, 3, 2, 1
# Download will only happen once as long as the file exists
if (!exists("states")){
  download_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                dir = raw_dir_us,
                layer = "cb_2016_us_state_20m") 
  
  states <- st_read(file.path(raw_dir_us, "cb_2016_us_state_20m.shp")) %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

if (!exists("ecoregions_l4321")){
  if(!file.exists(file.path(raw_dir_ecoregionl4, 'us_eco_l4_no_st.shp'))) {
    # Download ecoregion level 4
    ecol4_shp <- file.path(raw_dir_ecoregionl4, 'us_eco_l4_no_st.shp')
    download_data("ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip",
                  raw_dir_ecoregionl4,
                  ecol4_shp,
                  'us_eco_l4_no_st',
                  raw_dir_ecoregionl4)
    }
  
  ecoregions_l4321 <- st_read(file.path(raw_dir_ecoregionl4, 'us_eco_l4_no_st.shp')) %>%
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
    st_write(., file.path(ecoregion_dir, 'us_eco_l4321.gpkg'),
             driver = 'GPKG', delete_layer = TRUE)
} else {
  ecoregions_l4321 <- st_read(file.path(ecoregion_dir, 'us_eco_l4321.gpkg'))
  }

# 50k Fishnet
if (!exists("fishnet_50k")) {
  if (!file.exists(file.path(fishnet_dir, "fishnet_50k.gpkg"))) {
    fishnet_50k <- sf::st_make_grid(states, cellsize = 50000, what = 'polygons') %>%
      sf::st_sf('geometry' = ., data.frame('fishid50k' = 1:length(.))) %>%
      sf::st_intersection(., st_union(states))
    
    sf::st_write(fishnet_50k,
                 file.path(fishnet_dir, "fishnet_50k.gpkg"),
                 driver = "GPKG")
    
  } else {
    fishnet_50k <- sf::st_read(file.path(fishnet_dir, "fishnet_50k.gpkg"))
  }
}

# Import and clean the MTBS polygons
if (!exists('mtbs_fire')) {
  
  mtbs_shp <- file.path(raw_dir_mtbs)
  if (!file.exists(mtbs_shp)) {
    file_download(file.path(raw_dir_mtbs, 'mtbs_perims_DD.shp'),
                  raw_dir_mtbs, "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip")}
  
  mtbs_fire <- st_read(dsn = file.path(raw_dir_mtbs),
                       layer = 'mtbs_perims_DD', quiet= TRUE) %>%
    filter(Year >= '2001') %>%
    st_transform(st_crs(states)) %>%
    mutate(discovery_date = ymd(paste(Year, StartMonth, StartDay, sep="-")),
           discovery_year = year(discovery_date),
           discovery_day = day(discovery_date),
           discovery_month = month(discovery_date),
           discovery_doy = yday(discovery_date)) %>%
    st_intersection(., st_union(states)) %>%
    rename_all(tolower) %>%
    dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy, acres) %>%
    # Below we are categorizing the fires as in the East or West based on the -97th parallel - which is what MTBS uses
    st_transform("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
    sfc_as_cols(., st_centroid(geometry)) %>%
    mutate(mtbs_region = ifelse(x < -97, 'West', 'East')) %>%
    dplyr::select(-x, -y) %>%
    st_transform(st_crs(states))
}

if(!file.exists(file.path(mtbs_dir, 'lvl4321_eco_mtbs.gpkg'))) {
  mtbs_ecoregion <- mtbs_fire %>%
    st_intersection(., ecoregions_l4321) %>%
    mutate(mtbs_burn_area = as.numeric(st_area(.))/1000000) 
  
  mtbs_ecoregion %>%
    st_write(., file.path(mtbs_dir, 'lvl4321_eco_mtbs.gpkg'), delete_layer = TRUE)
  
  system(paste0('aws s3 sync data' , ' ', s3_base))
  
} else {
  mtbs_ecoregion <- st_read(file.path(mtbs_dir, 'lvl4321_eco_mtbs.gpkg'))
}

# Import Landfire to class out agriculture
if(!file.exists(file.path(evt_dir, 'us_agriculture_1km.tif'))) {
  hdr <- file.path(ag_raw_dir, 'Grid', 'us_140evt', 'hdr.adf')
  if(!file.exists(hdr)) {
    dest <- file.path(raw_prefix,'US_140EVT_20180618.zip')
    download.file('https://www.landfire.gov/bulk/downloadfile.php?FNAME=US_140_mosaic-US_140EVT_20180618.zip&TYPE=landfire', 
                  dest)
    unzip(dest,exdir = raw_prefix)
    unlink(dest)
    assert_that(file.exists(hdr))
  }
  tiff <- file.path(evt_dir, 'us_140evt.tif')
  if(!file.exists(tiff)) {
    system(paste0("gdal_translate -of 'GTiff' ", hdr, " ", tiff))
    us_140evt <- raster::raster(tiff)
  } else {
    us_140evt <- raster::raster(tiff)
  }
  ag_rst <- us_140evt %>%
    raster::reclassify(., matrix(c(-Inf, 3600, 0,
                                3600, 4000, 1, 
                                4000, Inf, 0), ncol = 3, byrow = TRUE))
  ag_rst_1km <- aggregate(ag_rst, fact=33, fun = modal)
  writeRaster(ag_rst_1km, file.path(evt_dir, 'us_agriculture_1km.tif'))
} else {
  ag_rst_1km <- raster::raster(file.path(evt_dir, 'us_agriculture_1km.tif'))
  }
  
# Import and polygonize the modis event data
if(!file.exists(file.path(fired_dir, 'usa_fired_2001_2017.gpkg'))) {
  yr_list <- rep(2001:2017)
  
  library(foreach)
  library(doParallel)
  
  cl <- parallel::makeCluster(parallel::detectCores()/2)
  doParallel::registerDoParallel(cl)
  
  full_poly <- foreach(y = unique(yr_list), .combine = rbind, .packages = c('foreach')) %dopar% {
    rst_list <- list.files(yearly_events_dir, pattern = paste0(y), full.names = TRUE)
    
    full_df <- foreach(x = rst_list, .combine = cbind, .packages = c('foreach', 'tidyverse', 'raster', 'sf', 'velox', 'stars')) %do% {
      
      file_basename <- x %>%
        basename %>%
        strsplit(split = "\\.") %>%
        unlist
      
      file_split <- file_basename[1] %>%
        strsplit(split = "_") %>%
        unlist
      
      yr <- file_split[3]
      var <- file_split[4]
      
      rst <- raster(x) 
      rst[rst == 0] <- NA
      
      poly_out <- rst %>%
        st_as_stars() %>%
        st_as_sf() %>%
        mutate(year = yr) 
      
      if(var == 'events') {
        poly_out <- poly_out %>%
          mutate(events = paste0(layer, '_', year)) %>%
          dplyr::select(events, year)
      } else if('layer' %in% colnames(poly_out)) {
        poly_out <- poly_out %>%
          dplyr::select(!!ensym(var) := layer, year)
      } else {
        poly_out <- poly_out %>%
          dplyr::select(!!ensym(var) := file_basename[1], year)
      }
      return(poly_out)
    }
    return(full_df)
  }
  parallel::stopCluster(cl)
  
  usa_fired <- full_poly %>%
    dplyr::select(-contains('.')) %>%
    group_by(events) %>%
    summarise_all(mode)
  st_write(usa_fired, file.path(fired_dir, 'usa_fired_2001_2017.gpkg'))
} else {
  usa_fired <- st_read(file.path(fired_dir, 'usa_fired_2001_2017.gpkg'))
}
  
# Spatially join ecoregions to the FIRED 
if (!file.exists(file.path(fired_dir, 'lvl4321_eco_fired.gpkg'))) {
  fired_ecoregion <- usa_fired %>%
    st_intersection(., ecoregions_l4321) %>%
    mutate(fired_burn_area = as.numeric(st_area(.))/1000000) 
  
  st_write(fired_ecoregion, file.path(fired_dir, 'lvl4321_eco_fired.gpkg'), delete_layer = TRUE)
} else {
  fired_ecoregion <- st_read(file.path(fired_dir, 'lvl4321_eco_fired.gpkg'))
}
