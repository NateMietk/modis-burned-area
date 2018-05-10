library(plotKML)


holloway <- mtbs_fire %>%
  st_join(., usa) %>%
  filter(stusps == 'NV') %>%
  filter(fire_name == 'HOLLOWAY') 

holloway_ms <- holloway %>%
  st_transform(p4string_ms)

# Vector of MODIS tiles to download
tiles <- get_tiles(holloway)

holloway_sp <- as(holloway, 'Spatial')
plotKML(holloway_sp["fire_name"], filename = "holloway.kml")

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

C5_holloway_dir <- file.path('data', 'testing', 'C5', 'holloway')

C5_hdf_list <- list.files(C5_holloway_dir,
                          full.names = TRUE,
                          pattern = 'hdf')

for (i in 1:length(C5_hdf_list)) {
  sds <- get_subdatasets(C5_hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(C5_holloway_dir, paste0("holloway_C5_", i, ".tif")))
}

raw_list5 <- list.files(file.path(C5_holloway_dir),
                       full.names = TRUE,
                       pattern = 'holloway_C5_')

holloway_raw5 <- stack(raw_list5) %>%
  crop(as(holloway_ms, 'Spatial')) %>%
  mask(as(holloway_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
holloway_raw5[holloway_raw5 < 1] <- NA
holloway_raw5[holloway_raw5 > 366] <- NA

holloway_long5 <- as.tibble(as.data.frame(holloway_raw5, xy = TRUE)) %>%
  rename(
    jan = holloway_C5_1,
    feb = holloway_C5_2,
    march = holloway_C5_3,
    april = holloway_C5_4,
    may = holloway_C5_5,
    june = holloway_C5_6,
    july = holloway_C5_7,
    august = holloway_C5_8,
    september = holloway_C5_9,
    october = holloway_C5_10,
    november = holloway_C5_11,
    december = holloway_C5_12
  ) %>%
  gather(month, burn_date, -x, -y) 

holloway_long5 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

holloway_spdf5 <- as(holloway_raw5, "SpatialPixelsDataFrame") 
holloway_df5 <- as.data.frame(holloway_spdf5) %>%
  dplyr::select(holloway_C5_8, x, y)
colnames(holloway_df5) <- c("value", "x", "y")

library(viridis)  # better colors for everyone
holloway_c5 <- ggplot() +  
  geom_tile(data=holloway_df5, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(holloway, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('Holloway fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))


# C6 ----------------------------------------------------------------------

C6_holloway_dir <- file.path('data', 'testing', 'C6', 'holloway')

C6_hdf_list <- list.files(C6_holloway_dir,
                          full.names = TRUE,
                          pattern = 'hdf')

for (i in 1:length(C6_hdf_list)) {
  sds <- get_subdatasets(C6_hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(C6_holloway_dir, paste0("holloway_C6_", i, ".tif")))
}

raw_list6 <- list.files(file.path(C6_holloway_dir),
                       full.names = TRUE,
                       pattern = 'holloway_C6_')

holloway_raw6 <- stack(raw_list6) %>%
  crop(as(holloway_ms, 'Spatial')) %>%
  mask(as(holloway_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
holloway_raw6[holloway_raw6 < 1] <- NA
holloway_raw6[holloway_raw6 > 366] <- NA

holloway_thresheld6 <- holloway_raw6
holloway_thresheld6[holloway_thresheld6 < 216] <- NA
holloway_thresheld6[holloway_thresheld6 > 232] <- NA

holloway_long6 <- as.tibble(as.data.frame(holloway_raw6, xy = TRUE)) %>%
  rename(
    jan = holloway_C6_1,
    feb = holloway_C6_2,
    march = holloway_C6_3,
    april = holloway_C6_4,
    may = holloway_C6_5,
    june = holloway_C6_6,
    july = holloway_C6_7,
    august = holloway_C6_8,
    september = holloway_C6_9,
    october = holloway_C6_10,
    november = holloway_C6_11,
    december = holloway_C6_12
  ) %>%
  gather(month, burn_date, -x, -y) 

holloway_long6 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

holloway_spdf6 <- as(holloway_raw6, "SpatialPixelsDataFrame") 
holloway_df6 <- as.data.frame(holloway_spdf6) %>%
  dplyr::select(holloway_C6_8, x, y)
colnames(holloway_df6) <- c("value", "x", "y")

library(viridis)  # better colors for everyone
holloway_c6 <- ggplot() +  
  geom_tile(data=holloway_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(holloway, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('Holloway fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="none") 



#  Cleaned and final plots ----------------------------------------------------------

hollowayT_spdf6 <- as(holloway_thresheld6, "SpatialPixelsDataFrame") 
hollowayT_df6 <- as.data.frame(hollowayT_spdf6) %>%
  dplyr::select(holloway_C6_8, x, y)
colnames(hollowayT_df6) <- c("value", "x", "y")


library(viridis)  # better colors for everyone
holloway_c6 <- ggplot() +  
  geom_tile(data=hollowayT_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_contour(data=hollowayT_df6, aes(x=x, y=y, z=value), bins = 8, colour = 'black') +
  geom_polygon(data=as(holloway, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('Holloway fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))





library(gridExtra)
grid.arrange(holloway_c5, holloway_c6, nrow = 1)

