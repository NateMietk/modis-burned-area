
holloway <- mtbs_fire %>%
  st_join(., usa) %>%
  filter(stusps == 'NV') %>%
  filter(fire_name == 'HOLLOWAY') 

# C6 ----------------------------------------------------------------------

fire_year <- holloway$discovery_year
fire_doy <- holloway$discovery_doy
holloway_burnarea <- list.files(yearly_composites, full.names = TRUE, pattern = paste0(fire_year, '.tif'))

holloway_rst <- raster::raster(holloway_burnarea) %>%
  raster::crop(holloway) %>%
  raster::mask(holloway)
holloway_rst[holloway_rst < (fire_doy-5)] <- NA

ggR(holloway_rst, geom_raster = TRUE, sat = 10, hue = 5) +
  geom_sf(data = holloway, color = "black", lwd=0.5, fill=NA) +
  scale_fill_distiller('Day of year', palette = 'RdBu', na.value = NA) +
  coord_sf(crs = st_crs(holloway), datum = NA) + 
  labs(x="",y="") +
  theme_bw() +
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank(),
    axis.title=element_blank(),
    axis.text=element_blank(),
    axis.ticks=element_blank())
