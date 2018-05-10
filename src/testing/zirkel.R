library(plotKML)

zirkel <- mtbs_fire %>%
  filter(fire_name == 'MT. ZIRKEL COMPLEX (BURN RIDGE)' | fire_name == 'MT. ZIRKEL COMPLEX (HINMAN)') 

zirkel_ms <- zirkel %>%
  st_transform(p4string_ms)

# Vector of MODIS tiles to download
tiles <- get_tiles(zirkel)

zirkel_sp <- as(zirkel, 'Spatial')
plotKML(zirkel_sp["fire_name"], filename = "zirkel.kml")

# Plotting function -------------------------------------------------------

theme_pub <- function(base_size=11, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=11),
            legend.position = "right",
            legend.text = element_text(size=11),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "white"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 11),
            axis.text.x = element_text(size = 10, angle = 65, hjust = 1),
            axis.text.y = element_text(size = 11)))
}

# C5 ----------------------------------------------------------------------

C5_zirkel_dir <- file.path('data', 'testing', 'C5', 'zirkel')

C5_hdf_list <- list.files(C5_zirkel_dir,
                          full.names = TRUE,
                          pattern = 'hdf')

for (i in 1:length(C5_hdf_list)) {
  sds <- get_subdatasets(C5_hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(C5_zirkel_dir, paste0("zirkel_C5_", i, ".tif")))
}

raw_list5 <- list.files(file.path(C5_zirkel_dir),
                        full.names = TRUE,
                        pattern = 'zirkel_C5_')

zirkel_raw5 <- stack(raw_list5) %>%
  crop(as(zirkel_ms, 'Spatial')) %>%
  mask(as(zirkel_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
zirkel_raw5[zirkel_raw5 < 1] <- NA
zirkel_raw5[zirkel_raw5 > 366] <- NA

zirkel_long5 <- as.tibble(as.data.frame(zirkel_raw5, xy = TRUE)) %>%
  rename(
    jan = zirkel_C5_1,
    feb = zirkel_C5_2,
    march = zirkel_C5_3,
    april = zirkel_C5_4,
    may = zirkel_C5_5,
    june = zirkel_C5_6,
    july = zirkel_C5_7,
    august = zirkel_C5_8,
    september = zirkel_C5_9,
    october = zirkel_C5_10,
    november = zirkel_C5_11,
    december = zirkel_C5_12
  ) %>%
  gather(month, burn_date, -x, -y) 

zirkel_long5 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

zirkel_df5 <- as(zirkel_raw5, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(july = ifelse(is.na(zirkel_C5_7), 0 , zirkel_C5_7),
         august = ifelse(is.na(zirkel_C5_8), 0 , zirkel_C5_8),
         september = ifelse(is.na(zirkel_C5_9), 0 , zirkel_C5_9)
  ) %>%
  mutate(value = pmax(july, august, september)) %>% 
  dplyr::select(value, x, y) 

library(viridis)  # better colors for everyone
zirkel_c5 <- ggplot() +  
  geom_tile(data=zirkel_df5, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(zirkel, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('zirkel fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="none")

# C6 ----------------------------------------------------------------------

C6_zirkel_dir <- file.path('data', 'testing', 'C6', 'zirkel')

C6_hdf_list <- list.files(C6_zirkel_dir,
                          full.names = TRUE,
                          pattern = 'hdf')

for (i in 1:length(C6_hdf_list)) {
  sds <- get_subdatasets(C6_hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(C6_zirkel_dir, paste0("zirkel_C6_", i, ".tif")))
}

raw_list6 <- list.files(file.path(C6_zirkel_dir),
                        full.names = TRUE,
                        pattern = 'zirkel_C6_')

zirkel_raw6 <- stack(raw_list6) %>%
  crop(as(zirkel_ms, 'Spatial')) %>%
  mask(as(zirkel_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
zirkel_raw6[zirkel_raw6 < 1] <- NA
zirkel_raw6[zirkel_raw6 > 366] <- NA

zirkel_long6 <- as.tibble(as.data.frame(zirkel_raw6, xy = TRUE)) %>%
  rename(
    jan = zirkel_C6_1,
    feb = zirkel_C6_2,
    march = zirkel_C6_3,
    april = zirkel_C6_4,
    may = zirkel_C6_5,
    june = zirkel_C6_6,
    july = zirkel_C6_7,
    august = zirkel_C6_8,
    september = zirkel_C6_9,
    october = zirkel_C6_10,
    november = zirkel_C6_11,
    december = zirkel_C6_12
  ) %>%
  gather(month, burn_date, -x, -y) 

zirkel_long6 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

zirkel_df6 <- as(zirkel_raw6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(july = ifelse(is.na(zirkel_C6_7), 0 , zirkel_C6_7),
         august = ifelse(is.na(zirkel_C6_8), 0 , zirkel_C6_8),
         september = ifelse(is.na(zirkel_C6_9), 0 , zirkel_C6_9)
  ) %>%
  mutate(value = pmax(july, august, september)) %>% 
  dplyr::select(value, x, y) 


library(viridis)  # better colors for everyone
zirkel_c6<- ggplot() +  
  geom_tile(data=zirkel_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(zirkel, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('zirkel Canyon fire: June 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))

#  Cleaned and final plots ----------------------------------------------------------
zirkel_thresheld6 <- zirkel_raw6
zirkel_thresheld6[zirkel_thresheld6 < 185] <- NA
zirkel_thresheld6[zirkel_thresheld6 > 244] <- NA

zirkelT_df6 <- as(zirkel_thresheld6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(july = ifelse(is.na(zirkel_C6_7), 0 , zirkel_C6_7),
         august = ifelse(is.na(zirkel_C6_8), 0 , zirkel_C6_8),
         september = ifelse(is.na(zirkel_C6_9), 0 , zirkel_C6_9)
  ) %>%
  mutate(value = pmax(august, july, september)) %>% 
  dplyr::select(value, x, y) 


library(viridis)  # better colors for everyone
zirkelT_c6 <- ggplot() +  
  geom_tile(data=zirkelT_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #geom_contour(data=zirkelT_df6, aes(x=x, y=y, z=value), colour = 'black') +
  geom_polygon(data=as(zirkel, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('zirkel fire: August 2002') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))


library(gridExtra)
grid.arrange(zirkel_c5, zirkel_c6, nrow = 1)

