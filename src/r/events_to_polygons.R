# functions ====================================================================
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

helper_function<- function(x) {
  # thanks http://rpubs.com/sogletr/sf-ops
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

pix_km2      <- function(p) p * 463.3127*463.3127 / 1000000
pix_acres    <- function(p) p * 463.3127*463.3127 * 0.000247105
pix_hectares <- function(p) p * 463.3127*463.3127 * 0.0001
  
# setup ========================================================================
libs <- c("tidyverse", "raster", "sf","fasterize", "cowplot", "ggpubr", 
          "segmented")

lapply(libs, library, character.only = TRUE, verbose = FALSE)

# paths ========================================================================
# local_data <- "/home/a/data"
# template_path <- "/home/a/data/MCD12Q1_mosaics/usa_lc_mosaic_2001.tif"
# ecoregion_path <- "home/a/data/background_ecoregions"
# s3_path <- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/delineated_events"
# cus_path <- "/home/a/data/background/CUS"
# cus_path <- 'data/shapefiles/conus.shp'
# raw_events_file <- "data/modis_burn_events_00_19.csv"
# landcover_eco_file <- "data/lc_eco_events.csv"
# lat_longs_file <- "data/ignition_lat_longs.csv"
template_path <- "data/rasters/landcover/mosaics/us_lc_mosaic_2001.tif"
ecoregion_path <- "data/shapefiles/ecoregion/us_eco_l3_modis.shp"
s3_path <- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/delineated_events"
cus_path <- 'data/shapefiles/conus.shp'
raw_events_file <- "data/tables/modis_burn_events_00_19.csv"
landcover_eco_file <- "data/tables/lc_eco_events.csv"
lat_longs_file <- "data/tables/ignition_lat_longs.csv"

# only requirement here is native modis projection (sinusiodal, 463.something resolution)
# this is for changing everything into the same projection
template <- raster(template_path)

# loading in fire event data frame
df <- read_csv(raw_events_file) %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2),
         year = as.numeric(substr(date, 1,4))) %>%
  # removing about 8000 repeat pixels (i.e. adjacent month detections)
  distinct(x,y,id, .keep_all = T)

# writing out polygon and attribute table ======================================

t0 <- Sys.time() # 15 min for creation, 30 secs to write
df_poly <- df %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  group_by(id)%>%
  # adding 1 m to the buffer to ensure things dissolve together nicely
  st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
  summarize(start_date = first(date))%>%
  mutate(final_perimeter = st_cast(.,"MULTILINESTRING") %>% st_length())
print(Sys.time()-t0)

st_write(df_poly, "data/modis_event_polygons.gpkg", delete_dsn=TRUE)
df_poly<-st_read("data/modis_event_polygons.gpkg")

# clipping to the continental US
cus <- st_read(cus_path) %>%
  summarise() %>%
  st_transform(st_crs(template))

st_intersects(df_poly, cus) -> x1
x2<- map_lgl(x1, helper_function)
df_cus <- df_poly[x2,]


# calculating event attributes, bringing in tables generated elsewhere
# lc is from "get_landcover_and_calculate_daily_stats.R"
# ll is from "get_lat_long_ignition_points.R"

lc <- read_csv(landcover_eco_file)
ll <- read_csv(lat_longs_file) %>%
  dplyr::select(id, -ignition_date, ignition_state,
                ignition_latitude = latitude,
                ignition_longitude = longitude)
event_attributes <- df %>%
  dplyr::select(-year) %>%
    group_by(id, date) %>%
  summarise(pixels = n()) %>%
  ungroup() %>%
  group_by(id) %>%
  # arranging by pixels is a hack to make it easier to pick the date of max growth
  arrange(desc(pixels)) %>% 
  summarise(total_pixels = sum(pixels),
            ignition_date = min(date),
            ignition_doy = strftime(ignition_date, format = "%j"),
            ignition_month = substr(ignition_date, 6,7),
            ignition_year = substr(ignition_date, 1,4),
            last_date = max(date),
            duration = as.numeric(last_date-ignition_date)+1,
            total_area_km2 = total_pixels %>% pix_km2(),
            total_area_acres = total_pixels %>% pix_acres(),
            total_area_ha = total_pixels %>% pix_hectares(), 
            fsr_pixels_per_day = total_pixels/as.numeric(duration), 
            fsr_km2_per_day = total_pixels/as.numeric(duration)%>% pix_km2(),
            fsr_acres_per_day = total_pixels/as.numeric(duration)%>% pix_acres(),
            fsr_ha_per_day = total_pixels/as.numeric(duration) %>% pix_hectares(),
            max_growth_pixels = max(pixels),
            max_growth_km2 = max(pixels) %>% pix_km2(),
            max_growth_acres = max(pixels)%>% pix_acres(),
            max_growth_ha = max(pixels)%>% pix_hectares(),
            min_growth_pixels = min(pixels),
            min_growth_km2 = min(pixels)%>% pix_km2(),
            min_growth_acres = min(pixels)%>% pix_acres(),
            min_growth_ha = min(pixels)%>% pix_hectares(),
            mean_growth_pixels = mean(pixels), 
            mean_growth_km2 = mean(pixels)%>% pix_km2(), 
            mean_growth_acres = mean(pixels)%>% pix_acres(), 
            mean_growth_ha = mean(pixels)%>% pix_hectares(), 
            max_growth_date = first(date)
            )%>%
  ungroup() %>%
  left_join(lc, by = "id") %>%
  left_join(ll, by = "id")




# checking to make sure they're the same
nrow(event_attributes)==nrow(df_poly)

# writing everything out =======================================================
write_csv(event_attributes, "data/event_attributes.csv")
system(paste("aws s3 cp data/event_attributes.csv",
             file.path(s3_path,"event_attributes.csv")))

st_write(left_join(dplyr::select(df_poly,-start_date), event_attributes), 
         "data/events_w_attributes.gpkg", delete_dsn = TRUE)
system(paste("aws s3 cp data/events_w_attributes.gpkg",
             file.path(s3_path,"modis_event_polygons.gpkg")))

st_write(left_join(dplyr::select(df_cus,-start_date), event_attributes), 
         "data/events_w_attributes_cus.gpkg", delete_dsn = TRUE)
system(paste("aws s3 cp data/events_w_attributes_cus.gpkg",
             file.path(s3_path,"modis_event_polygons_cus.gpkg")))


# daily level polygons =========================================================

# takes 1.5-2 hours on Adam's laptop
t0 <- Sys.time()
df <- read_csv(raw_events_file) %>%
  dplyr::select(id,date,x,y) %>%
  distinct(x,y,id, .keep_all = T) %>%
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2)) %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  group_by(id, date) %>%
  st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
  summarize(pixels = n(),
            area_km2 = n() %>% pix_km2(),
            area_acres = n()%>% pix_acres(),
            area_ha = n()%>% pix_hectares())
print(Sys.time() - t0)

st_write(df, "data/daily_polygons.gpkg")
system(paste("aws s3 cp data/daily_polygons.gpkg",
             file.path(s3_path, "daily_polygons.gpkg")))
print(Sys.time() - t0)

