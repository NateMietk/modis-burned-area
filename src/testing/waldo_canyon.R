library(plotKML)

waldo <- mtbs_fire %>%
  filter(fire_name == 'WALDO CANYON') 

waldo_ms <- waldo %>%
  st_transform(p4string_ms)

# Vector of MODIS tiles to download
tiles <- get_tiles(waldo)

waldo_sp <- as(waldo, 'Spatial')
plotKML(waldo_sp["fire_name"], filename = "waldo.kml")

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

waldo_list <- list.files(file.path(yearly_events),
                       full.names = TRUE)

waldo_rst <- stack(waldo_list) %>%
  crop(as(waldo, 'Spatial')) %>%
  mask(as(waldo, 'Spatial')) 
waldo_rst[waldo_rst < 1] <- NA
waldo_rst[waldo_rst > max(waldo_rst)+1] <- NA
plot(waldo_rst[[12]])
plot(st_geometry(waldo), add = T)

waldo_long5 <- as.tibble(as.data.frame(waldo_raw5, xy = TRUE)) %>%
  rename(
    jan = waldo_C5_1,
    feb = waldo_C5_2,
    march = waldo_C5_3,
    april = waldo_C5_4,
    may = waldo_C5_5,
    june = waldo_C5_6,
    july = waldo_C5_7,
    august = waldo_C5_8,
    september = waldo_C5_9,
    october = waldo_C5_10,
    november = waldo_C5_11,
    december = waldo_C5_12
  ) %>%
  gather(month, burn_date, -x, -y) 

waldo_long5 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

waldo_df5 <- as(waldo_raw5, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(july = ifelse(is.na(waldo_C5_7), 0 , waldo_C5_7),
         june = ifelse(is.na(waldo_C5_6), 0 , waldo_C5_6)
  ) %>%
  mutate(value = pmax(july, june)) %>% 
  dplyr::select(value, x, y) 

library(viridis)  # better colors for everyone
waldo_c5 <- ggplot() +  
  geom_tile(data=waldo_df5, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(waldo, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('waldo fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="none")

# C6 ----------------------------------------------------------------------

C6_waldo_dir <- file.path('data', 'testing', 'C6', 'waldo')

C6_hdf_list <- list.files(C6_waldo_dir,
                          full.names = TRUE,
                          pattern = 'hdf')

for (i in 1:length(C6_hdf_list)) {
  sds <- get_subdatasets(C6_hdf_list[i])
  gdal_translate(sds[1], dst_dataset = file.path(C6_waldo_dir, paste0("waldo_C6_", i, ".tif")))
}

raw_list6 <- list.files(file.path(C6_waldo_dir),
                        full.names = TRUE,
                        pattern = 'waldo_C6_')

waldo_raw6 <- stack(raw_list6) %>%
  crop(as(waldo_ms, 'Spatial')) %>%
  mask(as(waldo_ms, 'Spatial')) %>%
  projectRaster(crs = p4string_ea, res = 500)
waldo_raw6[waldo_raw6 < 1] <- NA
waldo_raw6[waldo_raw6 > 366] <- NA

waldo_long6 <- as.tibble(as.data.frame(waldo_raw6, xy = TRUE)) %>%
  rename(
    jan = waldo_C6_1,
    feb = waldo_C6_2,
    march = waldo_C6_3,
    april = waldo_C6_4,
    may = waldo_C6_5,
    june = waldo_C6_6,
    july = waldo_C6_7,
    august = waldo_C6_8,
    september = waldo_C6_9,
    october = waldo_C6_10,
    november = waldo_C6_11,
    december = waldo_C6_12
  ) %>%
  gather(month, burn_date, -x, -y) 

waldo_long6 %>%
  transform(month = factor(month, levels=c('jan', 'feb', 'march', 'april', "may", "june", 'july', 'august', 'september', 'october', 'november', "december"))) %>%
  ggplot() +
  geom_histogram(aes(x = burn_date, fill = month),position="identity", colour="grey40", bins = 50) +
  facet_wrap(~ month, scales = 'free') +
  theme_pub()

waldo_df6 <- as(waldo_raw6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(july = ifelse(is.na(waldo_C6_7), 0 , waldo_C6_7),
         june = ifelse(is.na(waldo_C6_6), 0 , waldo_C6_6)
  ) %>%
  mutate(value = pmax(june, july)) %>% 
  dplyr::select(value, x, y) 

library(viridis)  # better colors for everyone
waldo_c6 <- ggplot() +  
  geom_tile(data=waldo_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(waldo, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('waldo Canyon fire: June 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(5, "cm"))


#  Cleaned and final plots ----------------------------------------------------------
waldo_thresheld6 <- waldo_raw6
waldo_thresheld6[waldo_thresheld6 < 161] <- NA
waldo_thresheld6[waldo_thresheld6 > 186] <- NA

waldoT_df6 <- as(waldo_thresheld6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(june = ifelse(is.na(waldo_C6_6), 0 , waldo_C6_6),
         july = ifelse(is.na(waldo_C6_7), 0 , waldo_C6_7)) %>%
  mutate(value = pmax(june, july)) %>% 
  dplyr::select(value, x, y) 



library(viridis)  # better colors for everyone
waldoT_c6 <- ggplot() +  
  geom_tile(data=waldoT_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  #geom_contour(data=waldoT_df6, aes(x=x, y=y, z=value), colour = 'black') +
  geom_polygon(data=as(waldo, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('waldo fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))

library(gridExtra)
grid.arrange(waldo_c5, waldo_c6, nrow = 1)

