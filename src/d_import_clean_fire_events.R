#Download the 1984-2015 MTBS fire polygons -------------------------
mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
}

#Clean and prep the MTBS data to match the FPA database naming convention
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



