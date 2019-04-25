
classify_ba <-  function(x) {
  ifelse(x < 1, "< 1",
         ifelse(x >= 0.01 & x < 10, "1 - 10",
                ifelse(x >= 10 & x < 30, "10 - 30",
                       ifelse(x >= 30 & x < 50, "30 - 50",
                              "> 50"))))
}

classify_freq <-  function(x) {
  ifelse(x >= 1 & x <= 25, "1 - 25",
         ifelse(x >= 25 & x < 100, "25 - 100",
                ifelse(x >= 100 & x < 250, "100 - 250",
                       ifelse(x >= 250 & x < 500, "250 - 500",
                              "> 500"))))
}

mtbs_fish <- fishnet_50k %>%
  st_intersection(., mtbs_fire) %>%
  mutate(areakm2 = as.numeric(st_area(.))/1000000) %>%
  group_by(fishid50k) %>%
  summarise(freq = n(),
            burn_area = sum(areakm2)) %>%
  st_centroid() %>%
  mutate(class_size = classify_ba(burn_area),
         class_freq = classify_freq(freq))

modis_fish <- fishnet_50k %>%
  st_intersection(., usa_fired) %>%
  mutate(areakm2 = as.numeric(st_area(.))/1000000) %>%
  group_by(fishid50k) %>%
  summarise(freq = n(),
            burn_area = sum(areakm2)) %>%
  st_centroid() %>%
  mutate(class_size = classify_ba(burn_area),
         class_freq = classify_freq(freq))

fishnet_50k_sp <- as(st_centroid(fishnet_50k), "Spatial")
fs50_df <- SpatialPointsDataFrame(fishnet_50k_sp, fishnet_50k_sp@data)
fs50_df$id <- row.names(fs50_df)
fs50_df <- data.frame(fs50_df)

usa <- as(states, "Spatial")
usa$id <- row.names(usa)
st_df <- fortify(usa, region = 'id')
st_df <- left_join(st_df, usa@data, by = 'id')
names(st_df) <- tolower(names(st_df))

modis_fish_ff <- left_join(fs50_df, modis_fish, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

mtbs_fish_ff <- left_join(fs50_df, mtbs_fish, by = "fishid50k") %>%
  mutate(long = coords.x1,
         lat = coords.x2) %>%
  dplyr::select(-coords.x1, -coords.x2) 

p1 <- modis_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  transform(class_freq = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_size = factor(class_size, levels=c("< 1", "1 - 10", "10 - 30","30 - 50","> 50"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = class_freq, size = class_size)) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(5,"RdYlBu"))) +
  scale_size_discrete(range = c(.25, 2), name="Total burned area (km2)") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p2 <- mtbs_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  transform(class_freq = factor(class_freq, levels=c("1 - 25", "25 - 100","100 - 250", "250 - 500", "> 500"))) %>%
  transform(class_size = factor(class_size, levels=c("< 1", "1 - 10", "10 - 30","30 - 50","> 50"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = class_freq, size = class_size)) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(5,"RdYlBu"))) +
  scale_size_discrete(range = c(.25, 2), name="Total burned area (km2)") +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"))

p1l <- p1 + theme(legend.position="none")
p2l <- p2 + theme(legend.position="none")

grid.arrange(p1, p2, nrow = 2)
g <- arrangeGrob(p1l, p2l, nrow = 2) #generates g

ggsave(file = file.path(draft_figs_dir, "fired_vs_mtbs.tiff"), g, width = 10, height = 9, dpi = 600, scale = 3, units = "cm") #saves g


