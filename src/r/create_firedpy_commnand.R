library(sf)
library(tidyverse)

ak_tiles <- st_read("data/ak_tiles.gpkg") %>%
  st_set_geometry(NULL) %>%
  dplyr::select(-cat) %>%
  mutate_if(is.numeric,function(x)str_pad(x,width=2, side="left",pad= "0")) %>%
  mutate(tile = paste0("h",h,"v",v)) %>%
  pull(tile)

# these are the tiles for alaska
#firedpy -tiles h06v03 h07v03 h08v03 h09v03 h10v03 h09v02 h11v03 h10v02 h11v02 h12v02 h13v02 -proj_dir proj

# then we optimize and apply to canada