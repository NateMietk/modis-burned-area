# rim fire comparison figure
library(tidyverse)
library(sf)
library(ggpubr)

p_path <- "/home/a"

FIRED_p <- st_read(file.path(p_path, "rim_FIRED_perimeter.gpkg"))
gfa_p <- st_read(file.path(p_path, "rim_gfa_perimeter.gpkg")) %>%
  st_transform(crs = st_crs(FIRED_p))

ggarrange(
  ggplot()+
    geom_sf(data = gfa_p, aes(fill = as.factor(fire_ID))) +
    scale_fill_discrete(name = "Fire ID") +
    ggtitle("Rim Fire: Global Fire Atlas"),
  
  ggplot()+
    geom_sf(data = FIRED_p, aes(fill = as.factor(id))) +
    scale_fill_discrete(name = "Fire ID")+
    ggtitle("Rim Fire: FIRED")
  , 
  ncol = 1, nrow=2) +
  ggsave("rim_fire_comparison.png", dpi = 600, height = 10, width = 8)
