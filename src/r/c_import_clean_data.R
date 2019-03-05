# Import and prep the USA shapefile
if (!exists("states")){
  states <- download_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                          dir = us_prefix,
                          layer = "cb_2016_us_state_20m") %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

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
  
  
