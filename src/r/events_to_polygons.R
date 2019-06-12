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

# only requirement here is native modis projection (sinusiodal, 463.something resolution)
# this is for changing everything into the same projection
template <- raster(file.path(template_path, "usa_lc_mosaic_2001.tif"))

# loading ecoregion polygon, converting to raster
ecoregions <- st_read(file.path(ecoregion_path, "us_eco_l3.shp")) %>%
  mutate(NA_L1CODE = as.numeric(NA_L1CODE),
         l1_ecoregion = str_to_title(NA_L1NAME)) %>%
  st_transform(crs=crs(template, asText=TRUE)) %>%
  dplyr::select(NA_L1CODE, l1_ecoregion)

e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")

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

st_write(df_poly, "modis_event_polygons.gpkg")


event_attributes <- df %>%
  dplyr::select(-x,-y,-year) %>%
  group_by(id, date) %>%
  summarise(pixels = n()) %>%
  ungroup() %>%
  group_by(id) %>%
  summarise(total_pixels = sum(pixels),
         ignition_date = min(date),
         last_date = max(date),
         duration = as.numeric(last_date-ignition_date)+1,
         simple_fsr_pixels = total_pixels/as.numeric(duration), 
         total_area_km2 = total_pixels * 463*463/1000000,
         simple_fsr_km2 = total_pixels/as.numeric(duration)* 463*463/1000000)%>%
  ungroup()

# checking to make sure they're the same
nrow(event_attributes)==nrow(df_poly)

write_csv(event_attributes, "event_attributes.csv")

st_write(left_join(dplyr::select(df_poly,-start_date), event_attributes), "events_w_attributes.gpkg")
