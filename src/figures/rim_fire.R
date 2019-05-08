
library(rvest)
library(httr)
library(pbapply)
library(stringi)
library(tidyverse)
library(RCurl)
library(sf)
base_url <- "https://rmgsc.cr.usgs.gov/outgoing/GeoMAC/2013_fire_data/California/Rim"
r <- read_html(base_url)
zips <- grep("zip$", html_attr(html_nodes(pg, "a[href^='/outgoing/']"), "href"), value=TRUE)

pblapply(zips, function(zip_file, out_dir) {
  filename <- zip_file %>%
    str_split('/') %>%
    lapply(`[`, 7) %>%
    unlist()
  download.file(url = paste0('https://rmgsc.cr.usgs.gov', zip_file), destfile = paste0(out_dir, filename), method='auto')
  }, out_dir = 'data/fire/rim_fire/')

shp_list <- list.files('data/fire/rim_fire', pattern = '.shp$', full.names = TRUE)

rim_fire <- pblapply(shp_list, function(x) {
  shp <- sf::st_read(x)
})

rim_fire <- do.call('rbind', rim_fire) %>%
  group_by(DATE_) %>%
  summarise(ACRES = sum(ACRES))

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))

rim_calibrated <- NULL
for(i in 1:nrow(rim_fire)) {
  prior <- rim_fire[i,]
  current <- rim_fire[i+1,]
  
  rim_calibrated[[i]] <- st_erase(current, prior)
}

rim_fire_daily <- do.call('rbind', rim_calibrated) %>%
  rbind(rim_fire[1,]) %>%
  st_transform(p4string_ea) %>%
  st_set_precision(., 1000) %>%
  st_snap(., ., tolerance = 1) %>%
  lwgeom::st_make_valid() %>%
  st_cast("MULTIPOLYGON") 

tmp <- rim_fire_daily %>%
  mutate(doy = lubridate::yday(DATE_))

sf::st_write(rim_fire_daily, 'data/fire/rim_fire/rim_fire.gpkg', delete_layer = T)

tmp %>%
  ggplot() +
  geom_sf(aes(fill = doy))

tmp <- sf::st_read('data/fire/rim_fire/rim_fire.gpkg')

