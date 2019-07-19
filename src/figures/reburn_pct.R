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

ggarrange(p1, p2, legend = "bottom") +
  ggsave("images/reburn_fig.png", dpi = 300)
