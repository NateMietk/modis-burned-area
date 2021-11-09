# writing out a firedpy-created .csv to polygons
# this specific application is for an iteration of firedpy for north america
library(dplyr)
library(tidyverse)
library(sf)

# path to a random MCD64 file
template_path <- "/home/a/data/MCD12Q1_mosaics/usa_lc_mosaic_2001.tif"

# path to the output .csv from firedpy
# default path in firedpy/proj/outputs/tables
raw_events_file <- "data/fired_events_s1_t5_2020153.csv"

# come up with a descriptive name
output_fn <- "data/fired_NorthAmerica_s1_t5.gpkg"

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


t0 <- Sys.time() # 15 min for creation, 30 secs to write
df_poly <- df %>%
  st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE)) %>%
  group_by(id)%>%
  # adding 1 m to the buffer to ensure things dissolve together nicely
  st_buffer(dist = 1+(res(template)[1]/2), endCapStyle = "SQUARE")%>%
  dplyr::summarize()
print(Sys.time()-t0)


st_write(df_poly, output_fn, delete_dsn=TRUE)


