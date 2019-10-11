library(ggpubr)
source("src/r/a_prep_environment.R")
source("src/r/c_import_clean_data.R")

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
  st_buffer(dist=0)%>%
  summarise(freq = n(),
            burn_area = sum(areakm2)) %>%
  st_centroid() %>%
  mutate(class_size = classify_ba(burn_area),
         class_freq = classify_freq(freq))

system("aws s3 cp s3://earthlab-natem/modis-burned-area/delineated_events/modis_event_polygons_cus.gpkg data/modis/modis_event_polygons_cus.gpkg")
usa_fired <- st_read("data/modis/modis_event_polygons_cus.gpkg") %>%
  st_transform(crs = st_crs(fishnet_50k))

modis_fish <- fishnet_50k %>%
  st_intersection(., usa_fired) %>%
  mutate(areakm2 = as.numeric(st_area(.))/1000000) %>%
  group_by(fishid50k) %>%
  st_buffer(dist=0)%>%
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
  transform(class_freq = factor(class_freq, 
                                levels=c("1 - 25", "25 - 100","100 - 250", 
                                         "250 - 500", "> 500"))) %>%
  transform(class_size = factor(class_size, 
                                levels=c("< 1", "1 - 10", "10 - 30","30 - 50",
                                         "> 50"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black',
               fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = class_freq, size = class_size)) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(5,"RdYlBu")), 
                      name = "Fire Frequency") +
  scale_size_discrete(range = c(.25, 2.5), name=expression(Burned~Area~(km^2))) +
  theme_nothing(legend = TRUE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = c(.04,0),
        legend.justification = c(0,0),
        legend.background = element_rect(fill ="transparent"),
        legend.direction = "vertical",
        legend.box = "horizontal",
        legend.box.just = "bottom",
        legend.box.spacing = unit(1,"mm"))+
  guides(col = guide_legend(override.aes = list(shape = 15, size = 7)))
  

p2 <- mtbs_fish_ff %>%
  na.omit() %>%
  filter(freq != 0) %>%
  transform(class_freq = factor(class_freq, levels=c("1 - 25", "25 - 100",
                                                     "100 - 250", "250 - 500", 
                                                     "> 500"))) %>%
  transform(class_size = factor(class_size, levels=c("< 1", "1 - 10", "10 - 30",
                                                     "30 - 50","> 50"))) %>%
  ggplot() +
  geom_polygon(data = st_df, aes(x = long,y = lat,group=group), color='black', 
               fill = "gray97", size = .50)+
  geom_point(aes(x = long, y = lat, colour = class_freq, size = class_size)) +
  coord_equal() +
  scale_colour_manual(values = rev(brewer.pal(5,"RdYlBu"))) +
  scale_size_discrete(range = c(.25, 2.5)) +
  theme_nothing(legend = FALSE) +
  theme(plot.title = element_text(hjust = 0, size = 12),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank(),
        legend.key = element_rect(fill = "white"),
        legend.position = "none")

g <- ggarrange(p2,p1, nrow = 2, ncol = 1, labels = c("A. MTBS", "B. FIRED"),
               font.label = list(face = "bold", size = 16));g

ggsave(file = file.path(draft_figs_dir, "fired_vs_mtbs.pdf"), 
       g, width = 8.5, height = 11, dpi = 600, scale = 3, units = "cm") 

ggsave(file = file.path(draft_figs_dir, "fired_vs_mtbs.png"), 
       g, width = 8.5, height = 11, dpi = 300, scale = 3, units = "cm") 
        # legend.key = element_rect(fill = "white"))
