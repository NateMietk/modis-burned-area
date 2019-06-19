# get landcover and daily stats

# setup ------------------------------------------------------------------------
libs<- c("raster", "sf", "tidyverse", "fasterize")
lapply(libs, library, character.only =TRUE)

lc_path <- "/home/a/data/MCD12Q1_mosaics"
modis_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
ecoregion_path <- "/home/a/data/background/ecoregions"
template_path <- "/home/a/data/MCD12Q1_mosaics"
s3_path <- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/delineated_events"

getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# loading in data --------------------------------------------------------------
template <- raster(file.path(template_path, "usa_lc_mosaic_2001.tif"))

ecoregions <- st_read(file.path(ecoregion_path, "us_eco_l3.shp")) %>%
  mutate(NA_L1CODE = as.numeric(NA_L1CODE),
         l1_ecoregion = str_to_title(NA_L1NAME)) %>%
  st_transform(crs=modis_crs) %>%
  dplyr::select(NA_L1CODE, l1_ecoregion)

e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")

# loading in fire event data frame
df <- read_csv("data/modis_burn_events_00_19.csv") %>%
  dplyr::select(id,date,x,y) %>%
  #centering the pixels on the raster cells
  mutate(x = x + (res(template)[1]/2),
         y = y - (res(template)[1]/2),
         year = as.numeric(substr(date, 1,4))) %>%
  distinct(x,y,id, .keep_all = TRUE) %>%
  st_as_sf(coords = c("x","y"), crs = modis_crs)

# getting landcover, takes about 5 minutes
t0<-Sys.time()

ll<-list()

lc <- raster(file.path(lc_path, paste0("usa_lc_mosaic_",2001,".tif")))
ll[[1]] <- df[df$year == 2001,] %>%
  st_as_sf(coords = c("x","y"), crs = modis_crs)%>%
  mutate(lc = raster::extract(x=lc, y=.))

cc <- 2
years <- 2002:2018
t0<-Sys.time()
for(y in years){
  lc <- raster(file.path(lc_path, paste0("usa_lc_mosaic_",y-1,".tif")))
  ll[[cc]] <- df[df$year == y,] %>%
    st_as_sf(coords = c("x","y"), crs = modis_crs)%>%
    mutate(lc = raster::extract(x=lc, y=.))
  cc<-cc+1
}


lc <- raster(file.path(lc_path, paste0("usa_lc_mosaic_",2017,".tif")))
ll[[cc]] <- df[df$year == 2019,] %>%
  st_as_sf(coords = c("x","y"), crs = modis_crs)%>%
  mutate(lc = raster::extract(x=lc, y=.))

df_lc <- do.call("rbind", ll) %>%
  mutate(l1_eco = raster::extract(x=e_rast, y=.))
print(Sys.time() - t0)

#this takes forever to write... maybe it's not worth it
# st_write(df_lc, 
#          "data/modis_points_w_landcover_02_18.gpkg") 

labels <- st_set_geometry(ecoregions,NULL) %>%
  mutate(dup = duplicated(NA_L1CODE)) %>%
  filter(dup == FALSE) %>%
  dplyr::select(-dup) %>%
  dplyr::rename(l1_eco = NA_L1CODE)

lc_labels <- read_csv("data/usa_landcover_t1_classes.csv") %>%
  rename(lc = value, lc_name = name)

lc_only_events <- df_lc %>%
  st_set_geometry(NULL)  %>%
  group_by(id) %>%
  summarise(lc = getmode(lc),
            l1_eco = getmode(l1_eco)
            ) %>%
    ungroup()%>%
  left_join(labels) %>%
  left_join(lc_labels)

write_csv(lc_only_events, "data/lc_eco_events.csv")
system(paste("aws s3 cp data/lc_eco_events.csv",
             file.path(s3_path,"lc_eco_events.csv")))



daily <- df_lc %>%
  st_set_geometry(NULL) %>%
  group_by(id, date) %>%
  summarise(pixels = n(),
            l1_eco = getmode(l1_eco),
            lc = getmode(lc)) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(cum_pixels = cumsum(pixels),
         total_pixels = sum(pixels),
         ignition_date = min(date),
         last_date = max(date),
         duration = as.numeric(last_date-ignition_date)+1,
         simple_fsr_pixels = total_pixels/as.numeric(duration), 
         simple_fsr_km2 = total_pixels/as.numeric(duration)* 463*463/1000000)%>%
  ungroup()%>%
  mutate(daily_area_km2 = pixels * 463*463/1000000,
         cum_area_km2 = cum_pixels * 463*463/1000000,
         total_area_km2 = total_pixels * 463*463/1000000,
         pct_total_area = pixels/total_pixels*100,
         pct_cum_area = pixels/cum_pixels*100,
         event_day = as.numeric(date - ignition_date + 1),
         ratio_area_added_to_average = daily_area_km2/simple_fsr_km2,
         prior_pixels = cum_pixels-pixels,
         rel_fsr_per_day = ifelse(prior_pixels>0,pixels/prior_pixels, 0) 
         # need to decide on a value for that (the else value) if this ends up being useful
  ) %>%
  left_join(labels) %>%
  left_join(lc_labels) %>%
  filter(is.na(l1_eco) == F)

write_csv(daily,"data/daily_stats_w_landcover.csv")
system(paste("aws s3 cp data/daily_stats_w_landcover.csv",
             file.path(s3_path,"daily_stats_w_landcover.csv")))
