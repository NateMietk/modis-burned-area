# rim fire comparison figure
library(tidyverse)
library(sf)
library(ggpubr)

p_path <- "/home/a"

#add in mtbs
FIRED_p <- st_read(file.path(p_path, "rim_FIRED_perimeter.gpkg"))
gfa_p <- st_read(file.path(p_path, "rim_gfa_perimeter.gpkg")) %>%
  st_transform(crs = st_crs(FIRED_p))
FIRED_daily <- st_read(file.path(p_path, "projects", "modis-burned-area",
                                 "data", "daily_polygons.gpkg")) %>%
  filter(id == 57206)
mtbs_rim <- st_read(file.path(p_path, "data", "fire", "mtbs", "mtbs_perims_DD.shp")) %>%
  filter(Fire_Name == "RIM", Year == 2013) %>%
  st_transform(crs=st_crs(FIRED_p))
                      

ignition <- st_read(file.path(p_path, "projects", "modis-burned-area",
                              "data", "events_w_attributes.gpkg")) %>%
  st_set_geometry(NULL) %>%
  filter(id == 57206) %>%
  dplyr::select(lat=ignition_latitude, long=ignition_longitude) %>%
  st_as_sf(coords = c("long","lat"), 
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  st_transform(crs = st_crs(FIRED_p))


as.data.frame(sf::st_coordinates(ignition))-> ig
          
ggarrange(
  ggplot()+
    geom_sf(data = FIRED_p, aes(fill = as.factor(id))) +
    geom_sf(data = ignition, shape = 8, size = 4, stroke = 1.5) +
    scale_fill_discrete(name = "Fire ID")+
    geom_label(data = ig, 
               x=ig$X+2000,y=ig$Y, label = "Estimated Ingition", fontface = "bold",
               alpha = 0.5, hjust = "left")+
    ggtitle("Rim Fire: FIRED"), 
  ggplot()+
    geom_sf(data = gfa_p, aes(fill = as.factor(fire_ID))) +
    scale_fill_discrete(name = "Fire ID") +
    ggtitle("Rim Fire: Global Fire Atlas"),
  ggplot()+
    geom_sf(data = FIRED_daily, aes(fill = date)) +
    #colors from viridis::viridis(2)
    scale_fill_date(name = "Burn Date",low =  "#FDE725FF", high = "#440154FF") +
    ggtitle("Rim Fire: FIRED daily"),
  ggplot()+
    geom_sf(data = mtbs_rim, aes(fill = Fire_Name)) +
    ggtitle("Rim Fire: MTBS"),
  ncol =2, nrow=2, labels = "AUTO") +
  ggsave("images/rim_fire_comparison.png", dpi = 600, height = 11, width = 16) +
  ggsave("images/rim_fire_comparison_small.png", 
         dpi = 300, height = 11, width = 16, limitsize = TRUE)

