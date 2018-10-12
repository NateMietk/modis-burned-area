usa_outline <- st_union(usa)

states <- as(usa, "Spatial")
states$id <- row.names(states)
st_df <- fortify(states, region = 'id')
st_df <- left_join(st_df, states@data, by = 'id')
names(st_df) <- tolower(names(st_df))

###### Pixels
fsr_list <- list.files(yearly_events, pattern = 'fsr.tif', recursive = TRUE, full.names = TRUE)

fsr_raw <- raster::stack(fsr_list) 
fsr_rst <- fsr_raw %>%
  raster::reclassify(., cbind(-Inf, 0.0000000001, NA), right = FALSE)
names(fsr_rst) <- names(fsr_raw)

fsr_summed <- calc(fsr_rst, function(x) mean(x, na.rm = TRUE))

breaks_kmeans <- classInt::classIntervals(values(fsr_summed), n = 5, style = "kmeans")
breaks_kmeans <- round(breaks_kmeans$brks, 0)

p1 <- ggR(log(fsr_summed), geom_raster = T, sat = 10, hue = 5) +
  geom_polygon(data = st_df, aes(x = long, y = lat, group = group), 
               color='black', fill = "transparent", size = 0.1) +
  scale_fill_gradient2('log(FSR)', low="#1F77B4", high = '#D62728', 
                       midpoint = 2, na.value = NA) +
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

###### Level 1
fsr_shp_l1 <- ecoregl1 %>%
  left_join(., lvl1_mean_ci, by = 'na_l1name') %>%
  na.omit() 
breaks_kmeans <- classInt::classIntervals(fsr_shp_l1$Mean, n = 5, style = "kmeans")
breaks_kmeans <- round(breaks_kmeans$brks, 1)

p2 <- ggplot() +
  geom_sf(data = fsr_shp_l1, aes(fill = log(Mean)), color = NA, lwd = 0) +
  scale_fill_distiller('log Mean FSR', palette="RdBu", direction = -1) +
  geom_sf(data = usa_outline, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(fsr_shp_l4), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())

###### Level 4
fsr_shp_l4 <- ecoregions_l4 %>%
  left_join(., lvl4_mean_ci, by = 'US_L4NAME') %>%
  na.omit()
breaks_kmeans <- classInt::classIntervals(fsr_shp_l4$Mean, n = 5, style = "kmeans")
breaks_kmeans <- round(breaks_kmeans$brks, 0)

p3 <- ggplot() +
  geom_sf(data = fsr_shp_l4, aes(fill = log(Mean)), color = NA, lwd = 0) +
  scale_fill_distiller('log Mean FSR',palette="RdBu", direction = -1) +
  geom_sf(data = usa_outline, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(fsr_shp_l4), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())

g <- arrangeGrob(p1, p2, p3, ncol = 1) #generates g
ggsave(file = file.path('results', "fsr.pdf"), g, width = 6, height = 9, dpi = 600, scale = 3, units = "cm") #saves g
