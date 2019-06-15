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
cus_path <- "/home/a/data/background/CUS"

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
  summarize(start_date = first(date))%>%
  mutate(final_perimeter = st_cast(.,"MULTILINESTRING") %>% st_length())
print(Sys.time()-t0)

st_write(df_poly, "data/modis_event_polygons.gpkg", delete_dsn=TRUE)

cus <- st_read(cus_path) %>%
  summarise() %>%
  st_transform(st_crs(template))

st_intersects(df_poly, cus) ->x1
x2<- map_lgl(x1, unnecessary)
             
unnecessary<- function(x) {
  # thanks http://rpubs.com/sogletr/sf-ops
  if (length(x) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

df_cus <- df_poly[x2,]



event_attributes <- df %>%
  dplyr::select(-x,-y,-year) %>%
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
            total_area_km2 = total_pixels * 463.3127*463.3127/1000000,
            total_area_acres = total_pixels * 463.3127*463.3127 * 0.000247105,
            total_area_ha = total_pixels * 463.3127*463.3127 * 0.0001, 
            fsr_pixels_per_day = total_pixels/as.numeric(duration), 
            fsr_km2_per_day = total_pixels/as.numeric(duration)* 463.3127*463.3127/1000000,
            fsr_acres_per_day = total_pixels/as.numeric(duration)* 463.3127*463.3127 * 0.000247105,
            fsr_ha_per_day = total_pixels/as.numeric(duration) * 463.3127*463.3127 * 0.0001,
            max_growth_pixels = max(pixels),
            max_growth_km2 = max(pixels) * 463.3127*463.3127/1000000,
            max_growth_acres = max(pixels)* 463.3127*463.3127 * 0.000247105,
            max_growth_ha = max(pixels)* 463.3127*463.3127 * 0.0001,
            min_growth_pixels = min(pixels),
            min_growth_km2 = min(pixels)* 463.3127*463.3127/1000000,
            min_growth_acres = min(pixels)* 463.3127*463.3127 * 0.000247105,
            min_growth_ha = min(pixels)* 463.3127*463.3127 * 0.0001,
            mean_growth_pixels = mean(pixels), 
            mean_growth_km2 = mean(pixels)* 463.3127*463.3127/1000000, 
            mean_growth_acres = mean(pixels)* 463.3127*463.3127 * 0.000247105, 
            mean_growth_ha = mean(pixels)* 463.3127*463.3127 * 0.0001, 
            max_growth_date = first(date)
            )%>%
  ungroup()

# need mode landcover


# checking to make sure they're the same
nrow(event_attributes)==nrow(df_poly)

write_csv(event_attributes, "data/event_attributes.csv")

st_write(left_join(dplyr::select(df_poly,-start_date), event_attributes), 
         "data/events_w_attributes.gpkg", delete_dsn = TRUE)
system(paste("aws s3 cp data/events_w_attributes.gpkg",
             file.path(s3_path,"modis_event_polygons.gpkg")))

st_write(left_join(dplyr::select(df_cus,-start_date), event_attributes), 
         "data/events_w_attributes_cus.gpkg", delete_dsn = TRUE)
system(paste("aws s3 cp data/events_w_attributes_cus.gpkg",
             file.path(s3_path,"modis_event_polygons_cus.gpkg")))

e_w_a_lc<-st_read("data/events_w_attributes_cus.gpkg") %>%
  left_join(lc_only_events, by = "id")
st_write(e_w_a_lc, "data/events_w_attributes_cus.gpkg", delete_dsn = TRUE)

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

