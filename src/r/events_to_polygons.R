getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# setup ------------------------------------------------------------------------
libs <- c("tidyverse", "raster", "sf","fasterize", "cowplot", "ggpubr", 
          "segmented")

lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)

local_data <- "/home/a/data"
template_path <- "/home/a/data/MCD12Q1_mosaics"
ecoregion_path <- "home/a/data/background_ecoregions"
s3_path <- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/delineated_events"

# only requirement here is native modis projection (sinusiodal, 463.something resolution)
# this is for changing everything into the same projection
template <- raster(file.path(template_path, "usa_lc_mosaic_2001.tif"))


# loading in fire event data frame
df <- read_csv("data/modis_burn_events_00_19.csv") %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2),
         year = as.numeric(substr(date, 1,4)))

# writing out polygon and attribute table ======================================

t0 <- Sys.time() # 15 min for creation, 30 secs to write
df_poly <- df %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  group_by(id)%>%
  
  # adding 1 m to the buffer to ensure things dissolve together nicely
  st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
  summarize(start_date = first(date))
print(Sys.time()-t0)

st_write(df_poly, "data/modis_event_polygons.gpkg")


event_attributes <- df %>%
  dplyr::select(-x,-y,-year) %>%
  group_by(id, date) %>%
  summarise(pixels = n()) %>%
  ungroup() %>%
  group_by(id) %>%
  summarise(total_pixels = sum(pixels),
            ignition_date = min(date),
            ignition_doy = strftime(ignition_date, format = "%j"),
            ignition_month = substr(ignition_date, 6,7),
            ignition_year = substr(ignition_date, 1,4),
            last_date = max(date),
            duration = as.numeric(last_date-ignition_date)+1,
            total_area_km2 = total_pixels * 463.3127*463.3127/1000000,
            total_area_acres = total_pixels * 463.3127*463.3127 * 0.000247105,
            total_area_ha = total_pixels * 463.3127*463.3127 * 0.0001, 
            fsr_pixels_per_day = total_pixels/as.numeric(duration), 
            fsr_km2_per_day = total_pixels/as.numeric(duration)* 463.3127*463.3127/1000000,
            fsr_acres_per_day = total_pixels/as.numeric(duration)* 463.3127*463.3127 * 0.000247105,
            fsr_ha_per_day = total_pixels/as.numeric(duration) * 463.3127*463.3127 * 0.0001
            )%>%
  ungroup()

# checking to make sure they're the same
nrow(event_attributes)==nrow(df_poly)

write_csv(event_attributes, "data/event_attributes.csv")

st_write(left_join(dplyr::select(df_poly,-start_date), event_attributes), 
         "data/events_w_attributes.gpkg")
system(paste("aws s3 cp data/events_w_attributes.gpkg",
             file.path(s3_path,"modis_event_polygons.gpkg")))

# daily level ==================================================================

t0 <- Sys.time()
df <- read_csv("data/modis_burn_events_00_19.csv") %>%
  dplyr::select(id,date,x,y) %>%
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2)) %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  group_by(id, date) %>%
  st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
  summarize(pixels = n(),
            area_km2 = n() * 463.3127*463.3127/1000000,
            area_acres = n()* 463.3127*463.3127 * 0.000247105,
            area_ha = n() * 463.3127*463.3127 * 0.0001)
print(Sys.time() - t0)

st_write(df, "data/daily_polygons.gpkg")
system(paste("aws s3 cp data/daily_polygons.gpkg",
             file.path(s3_path, "daily_polygons.gpkg")))
print(Sys.time() - t0)

