
mendo <- st_read('data/fire/mendo/MendoComplexProgression.shp') %>%
  st_transform(st_crs(usa)) %>%
  st_join(., usa) %>%
  mutate(dates = lubridate::yday(Perimeter_)) %>%
  lwgeom::st_make_valid(.)

# C6 ----------------------------------------------------------------------
pal <- colorRampPalette( brewer.pal( 6 , "RdYlBu" ) )
mendo %>%
  ggplot() + 
  geom_sf(aes(fill = dates), color = "black") +
  coord_sf(crs = st_crs(mendo), datum = NA) + 
  theme_nothing(legend = TRUE) +
  scale_fill_viridis_c(option = "plasma") +
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_rect(fill = "white"))


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
