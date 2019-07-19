# making a figure illustrating reburns
libs<- c("tidyverse", "sf", "ggpubr")
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

bb <- st_bbox(modis_fires)

p1 <- ggplot(modis_fires) +
  geom_sf(aes(fill = id), alpha = 0.5) +
  geom_sf(data = mtbs_fires, fill = "transparent") +
  scale_fill_discrete(name = "FIRED ID")+
  ggtitle("A. FIRED events") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4])) +
  theme_void()

p2 <- ggplot(mtbs_fires) +
  geom_sf(aes(fill = Fire_Name), alpha = 0.5) +
  geom_sf(data = modis_fires,fill = "transparent") +
  scale_fill_discrete(name = "Fire Name")+
  ggtitle("B. MTBS events") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4])) +
  theme_void()

p3 <- ggplot(gfa_fires) +
  geom_sf(aes(fill = fire_ID), alpha = 0.5)+
  geom_sf(data = mtbs_fires, fill = "transparent") +
  scale_fill_discrete(name = "GFA ID", breaks = c("23880", "23898","23876", 
                                                  "23891", "24445", "24490"))+
  theme_void()+
 # theme(legend.position = "none") +
  ggtitle("A. Global Fire Atlas events (57 events)") +
  xlim(c(bb[1], bb[3])) +
  ylim(c(bb[2], bb[4]))

ggarrange(p1, p2, p3, legend = "bottom", ncol = 3, nrow=1) +
  ggsave("images/reburn_fig.png", dpi = 300, width = 12, heigh = 6)
