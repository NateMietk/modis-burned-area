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
  dplyr::select(fire_id, fire_name, discovery_date, discovery_year, discovery_day, discovery_month, discovery_doy) 

hdf_list <- list.files(file.path(version_dir, 'tmp'),
                       full.names = TRUE,
                       pattern = 'hdf')
for (i in 1:length(hdf_list)) {
  sds <- get_subdatasets(hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(version_dir, 'tmp', paste0("modis_", i, ".tif")))
}

raw_list <- list.files(file.path(version_dir, 'tmp'),
                       full.names = TRUE,
                       pattern = 'modis')

# Import and prep the USA shapefile and extract for only the Western US
co_ms <- st_read(file.path(us_prefix, "cb_2016_us_state_20m.shp"),
                 quiet = TRUE) %>%
  filter(STUSPS %in% c('CO')) %>%
  dplyr::select(STUSPS) %>%
  st_transform(p4string_ms)

co <- co_ms %>%
  st_transform(p4string_ea)

#
colorado_raw <- stack(raw_list) %>%
  crop(as(co_ms, 'Spatial')) %>%
  mask(as(co_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
colorado_raw[colorado_raw < 1] <- NA
colorado_raw[colorado_raw > 366] <- NA

colorado_df <- as.tibble(as.data.frame(colorado_raw, xy = TRUE)) %>%
  rename(
    feb = modis_1,
    march = modis_2,
    april = modis_3,
    may = modis_4,
    june = modis_5,
    july = modis_6,
    august = modis_7,
    september = modis_8,
    october = modis_9,
    november = modis_10,
    december = modis_11
  ) %>%
  na.omit() %>%
  gather(month, burn_date, -x, -y) %>%
  filter(burn_date > 1)

colorado_df %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november'))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

waldo <- mtbs_fire %>%
  filter(fire_name == 'WALDO CANYON') 

waldo_raw <- colorado_raw %>%
  crop(as(waldo, 'Spatial')) %>%
  mask(as(waldo, 'Spatial'))
waldo_raw[waldo_raw < 1] <- NA
waldo_raw[waldo_raw > 366] <- NA

waldo_composite <- raster("data/MCD64A1/C6/yearly_events/USA_burnevents_2012.tif") %>%
  crop(as(waldo, 'Spatial')) %>%
  mask(as(waldo, 'Spatial'))
waldo_composite[waldo_composite < 1] <- NA

waldo_df <- as.tibble(as.data.frame(waldo_raw, xy = TRUE)) %>%
  rename(
    jan = modis_1,
    feb = modis_2,
    march = modis_3,
    april = modis_4,
    may = modis_5,
    june = modis_6,
    july = modis_7,
    august = modis_8,
    september = modis_9,
    october = modis_10,
    november = modis_11
  ) %>%
  na.omit() %>%
  gather(month, burn_date, -x, -y) 

theme_pub <- function(base_size=11, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=11),
            legend.position = "right",
            legend.text = element_text(size=11),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 11),
            axis.text.x = element_text(size = 10, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 11)))
}

waldo_df %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november'))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

levelplot(waldo_raw, layers = 7:8, par.settings = RdBuTheme, names.attr = c('Waldo Canyon fire: May 2012', 'Waldo Canyon fire: June 2012')) + layer(sp.polygons(as(waldo, 'Spatial')))
levelplot(waldo_composite, par.settings = RdBuTheme, main = 'Fire events as classified for the Waldo Canyon fire \n (n = 26 discrete events)') + layer(sp.polygons(as(waldo, 'Spatial')))























# import all fire event tifs into raster stack
event_list <- list.files(file.path(version_dir, 'yearly_events'),
                         full.names = TRUE)

conus_events <- stack(event_list) %>%
  crop(as(usa, 'Spatial')) %>%
  mask(as(usa, 'Spatial'))
vx_events <- velox(conus_events)
vx_events_poly <- vx_events$rasterize(spdf, field="id", background = NA)

events <- stack(event_list[12]) %>%
  setMinMax(.)

events[events == 0] <- NA
polylist <- lapply(as.list(events), rasterToPolygons, dissolve= TRUE)




