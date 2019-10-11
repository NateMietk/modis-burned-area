# making a figure illustrating reburns
libs<- c("tidyverse", "sf", "ggpubr", "ggsn")
lapply(libs, library, character.only = TRUE)

modis_fires <- st_read("data/events_w_attributes_cus.gpkg") %>%
  filter(id == 25211 | id == 29790) %>%
  mutate(id = as.character(id))
mtbs_fires <- st_read("/home/a/data/fire/mtbs/mtbs_perims_DD.shp") %>%
  filter(Fire_Name == "MOONSHINE BAY" | Fire_Name == "SOUR ORANGE") %>%
  st_transform(crs = st_crs(modis_fires)) %>%
  mutate(Fire_Name = as.character(Fire_Name) %>% str_to_title())

gfa_fires <- st_read("data/gfa_reburn.gpkg") %>%
  st_transform(crs = st_crs(modis_fires)) %>%
  mutate(fire_ID = as.character(fire_ID)) 

ignitions <- st_read(file.path("/home/a", "projects", "modis-burned-area",
                              "data", "events_w_attributes.gpkg")) %>%
  st_set_geometry(NULL) %>%
  filter(id == 25211 | id == 29790) %>%
  dplyr::select(lat=ignition_latitude, long=ignition_longitude,id,
                ignition_date, last_date) %>%
  st_as_sf(coords = c("long","lat"), 
           crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  st_transform(crs = st_crs(modis_fires))

bb <- st_bbox(modis_fires)

ig <- ignitions %>%
  sf::st_coordinates() %>%
  as.data.frame() %>%
  mutate(id = c(25211,29790),
         X = c(-8040000,-8040000),
         Y = c(2970000, 2973000))%>%
  left_join(ignitions %>% st_set_geometry(NULL)) %>%
  mutate(label = paste0("Start: ", ignition_date, "\nEnd: ", last_date))

mtbs_labs <- mtbs_fires %>%
  st_set_geometry(NULL) %>%
  dplyr::select(Fire_Name, Year, StartMonth, StartDay) %>%
  mutate(label = paste0("Start: ", Year,"-", StartMonth,"-", StartDay),
         X = c(-8055000, -8055000),
         Y = c(2995000, 2997000))

north <- data.frame(x = c(-81.27, -81.27), y = c(26.95,26.975)) %>%
  st_as_sf(coords = c("x","y"), crs=4326) %>%
  st_transform(crs = st_crs(modis_fires)) %>%
  st_coordinates() %>%
  as.data.frame()

p1 <- ggplot(modis_fires) +
  geom_sf(aes(fill = id), alpha = 0.5) +
  geom_sf(data = mtbs_fires, fill = "transparent", aes(lty = Fire_Name),lwd=1) +
  scale_linetype_discrete(name = "MTBS Name")+
  scale_fill_discrete(name = "FIRED ID")+
  ggtitle("A. FIRED events") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4])) +
  geom_label(data = ig, 
             aes(x=X,y=Y, label = label, fill = as.factor(id)), fontface = "bold",
             alpha = 0.5, hjust = "left", size = 2.5, show.legend = FALSE)+
  theme_bw()+
  theme(axis.title = element_blank());p1
  

p2 <- ggplot(mtbs_fires) +
  geom_sf(aes(fill = Fire_Name), alpha = 0.5) +
  geom_sf(data = modis_fires,fill = "transparent") +
  scale_fill_discrete(name = "Fire Name")+
  ggtitle("B. MTBS events") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4])) +
  geom_label(data = mtbs_labs, 
             aes(x=X,y=Y, label = label, fill = as.factor(Fire_Name)), fontface = "bold",
             alpha = 0.5, hjust = "left", size = 2.5, show.legend = FALSE)+
  theme_bw()+
  theme(axis.title = element_blank())

p3 <- ggplot(gfa_fires) +
  geom_sf(aes(fill = fire_ID), alpha = 0.5)+
  geom_sf(data = mtbs_fires, fill = "transparent", aes(lty = Fire_Name),lwd=1, show.legend = FALSE) +
  #scale_linetype_discrete(name = "MTBS Name")+
  scale_fill_discrete(name = "GFA ID", breaks = c("23880","23891", "24490"))+
  theme_bw()+
  theme(axis.title = element_blank())+
  geom_segment(x=north$X[1], xend = north$X[2], 
               y=north$Y[1], yend = north$Y[2], 
               arrow = arrow(type = "closed", angle = 15,
                             length = unit(0.65, "inches")), 
               lwd = 2, color = "grey30")+
  geom_text(x=north$X[1], y = north$Y[1], label = "N",
            fontface="bold", color = "white", angle = -30)+
  scalebar(gfa_fires, dist = 5, dist_unit = "km", transform = FALSE, 
           st.dist = .03, st.color = "grey30", box.color = "grey30",  
           st.bottom = FALSE, box.fill = c("grey30", "white"), 
           location = "bottomright", st.size = 3)+
 # theme(legend.position = "none") +
  ggtitle("C. Global Fire Atlas events (57 events)") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4]));p3

ggarrange(p1, p3, legend = "bottom", ncol = 2, nrow=1) +
  ggsave("images/reburn_fig.png", dpi = 300, width = 12, height = 6) +
  ggsave("images/reburn_fig.pdf", dpi = 600, width = 12, height = 6) 
  

