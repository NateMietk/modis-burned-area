

# Import and clean the Level 1 Ecoregions
if (!exists('ecoregl1')) {
  if (!file.exists(file.path(ecoregion_out, 'us_eco_l1.gpkg'))) {
    
    # Download the Level 1 Ecoregions
    ecoregion_shp <- file.path(ecoregion_out, "NA_CEC_Eco_Level1.shp")
    if (!file.exists(ecoregion_shp)) {
      loc <- "ftp://newftp.epa.gov/EPADataCommons/ORD/Ecoregions/cec_na/na_cec_eco_l1.zip"
      dest <- paste0(ecoregion_out, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = ecoregion_out)
      unlink(dest)
      assert_that(file.exists(ecoregion_shp))
    }
    
    ecoregl1 <- st_read(dsn = ecoregion_out, layer = "NA_CEC_Eco_Level1") %>%
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
  }
}

# Import and clean the MTBS polygons
if (!exists('mtbs_fire')) {
  
    mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
  if (!file.exists(mtbs_shp)) {
    loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
    dest <- paste0(mtbs_prefix, ".zip")
    download.file(loc, dest)
    unzip(dest, exdir = mtbs_prefix)
    unlink(dest)
    assert_that(file.exists(mtbs_shp))
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

mtbs_ecoreg <- mtbs_fire %>%
  st_intersection(., ecoregl1)

mtbs_ecoreg_t <- mtbs_ecoreg %>%
  st_make_valid() %>%
  mutate(ClArea_m2 = as.numeric(st_area(Shape)))







