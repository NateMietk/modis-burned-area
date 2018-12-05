usa_outline <- st_union(usa)

###### Fire events
events_list <- list.files(yearly_events, pattern = 'events.tif', recursive = TRUE, full.names = TRUE)

events_raw <- raster::stack(events_list) 
events_rst <- events_raw %>%
  raster::reclassify(., cbind(-Inf, 1, NA), right = FALSE)
names(events_rst) <- names(events_raw)

fl <- usa %>%
  filter(stusps == 'FL')

events_rst_fl <- events_rst %>%
  crop(fl) %>%
  raster::mask(as(fl, 'Spatial')) 

p1 <- ggR(events_rst, geom_raster = TRUE, sat = 10, hue = 5) +
  geom_sf(data = usa, color = "black", lwd=0.1, fill=NA) +
  scale_fill_distiller('Event ID', palette = 'Reds', na.value = NA) +
  coord_sf(crs = st_crs(usa), datum = NA) + 
  labs(x="",y="") + ggtitle('Fire events for 2017') +
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
ggsave(file = file.path('results', "events_gridded.pdf"), p1, width = 6, height = 4, dpi = 600, scale = 2, units = "cm") #saves g

p1 <- ggR(events_rst_fl, geom_raster = TRUE, sat = 10, hue = 5) +
  geom_sf(data = fl, color = "black", lwd=0.1, fill=NA) +
  scale_fill_distiller('Event ID', palette = 'Reds', na.value = NA) +
  coord_sf(crs = st_crs(fl), datum = NA) + 
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
ggsave(file = file.path('results', "fl_events_gridded.pdf"), p1, width = 7, height = 4, dpi = 600, scale = 2, units = "cm") #saves g


###### Average fire spread rate
fsr_list <- list.files(yearly_events, pattern = 'fsr.tif', recursive = TRUE, full.names = TRUE)

fsr_raw <- raster::stack(fsr_list) 
fsr_rst <- fsr_raw %>%
  raster::reclassify(., cbind(-Inf, 0.0000000001, NA), right = FALSE)
names(fsr_rst) <- names(fsr_raw)

fsr_summed <- calc(fsr_rst, function(x) mean(x, na.rm = TRUE))

p1 <- ggR(log(fsr_summed), geom_raster = TRUE, sat = 10, hue = 5) +
  geom_sf(data = usa, color = "black", lwd=0.1, fill=NA) +
  scale_fill_distiller('log(Mean FSR)', palette = 'BrBG', na.value = NA) +
  coord_sf(crs = st_crs(usa), datum = NA) + 
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
ggsave(file = file.path('results', "fsr_gridded.pdf"), p1, width = 6, height = 4, dpi = 600, scale = 3, units = "cm") #saves g


###### Level 1
fsr_shp_l1 <- ecoregions_l4321 %>%
  ungroup() %>%
  group_by(na_l1name) %>%
  summarise() %>%
  left_join(., lvl1_mean_ci, by = 'na_l1name') %>%
  na.omit() %>%
  st_cast("MULTIPOLYGON")
breaks_kmeans <- classInt::classIntervals(fsr_shp_l1$Mean, n = 5, style = "kmeans")
breaks_kmeans <- round(breaks_kmeans$brks, 1)

p2 <- ggplot() +
  geom_sf(data = fsr_shp_l1, aes(fill = log(Mean)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Mean FSR)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = usa_outline, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(fsr_shp_l1), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', "fsr_level1.pdf"), p2, width = 6, height = 4, dpi = 600, scale = 3, units = "cm") #saves g

###### Level 3 MAX FSR

fsr_shp_l3 <- lvl3_eco_fsr_ts %>%
  group_by(us_l3name) %>%
  summarise(max_fsr = max(max_fsr, rm.na = TRUE)) %>%
  dplyr::select(-geom) %>%
  as.data.frame() %>%
  left_join(ecoregions_l4321, ., by = 'us_l3name') %>%
  group_by(us_l3name) %>%
  summarise(max_fsr = max(max_fsr))

p3 <- ggplot() +
  geom_sf(data = fsr_shp_l3, aes(fill = log(max_fsr)), color = NA, lwd = 0) +
  scale_fill_distiller('log(Max FSR))', palette = 'BrBG', na.value = NA) +
  geom_sf(data = ecoregl1, color = "black", lwd=0.1, fill=NA) +
  geom_sf(data = usa_outline, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(fsr_shp_l3), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank(),
    legend.position = 'bottom',
    legend.direction = "horizontal")
ggsave(file = file.path('results', "log_max_fsr_level3.pdf"), p3, width = 8, height = 5, dpi = 150, scale = 2, units = "cm") #saves g

fsr_shp_l3 %>%
  ggplot() +
  geom_histogram(aes(x= log(max_fsr)), binwidth = 1) +
  theme_pub()
###### Level 3

fsr_shp_l3 <- ecoregions_l4321 %>%
  group_by(us_l3name) %>%
  summarise() %>%
  left_join(., lvl3_mean_ci, by = 'us_l3name') %>%
  mutate(logMean = log(Mean)) %>%
  na.omit()

p3 <- ggplot() +
  geom_sf(data = fsr_shp_l3, aes(fill = logMean), color = NA, lwd = 0) +
  scale_fill_distiller('log(Mean FSR)', palette = 'BrBG', na.value = NA) +
  geom_sf(data = ecoregl1, color = "black", lwd=0.1, fill=NA) +
  coord_sf(crs = st_crs(fsr_shp_l3), datum = NA) + 
  theme(
    panel.ontop = TRUE,   ## Note: this is to make the panel grid visible in this example
    panel.grid = element_blank(), 
    line = element_blank(), 
    rect = element_blank(), 
    plot.background = element_blank())
ggsave(file = file.path('results', "fsr_level3.pdf"), p3, width = 6, height = 4, dpi = 600, scale = 3, units = "cm") #saves g

###### Level 4
fsr_shp_l4 <- ecoregions_l4321 %>%
  left_join(., lvl4_mean_ci, by = 'US_L4NAME') %>%
  na.omit()

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
