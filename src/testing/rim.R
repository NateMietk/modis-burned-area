library(plotKML)


rim <- mtbs_fire %>%
  filter(fire_id == 'CA3785712008620130817')

rim_ms <- rim %>%
  st_transform(p4string_ms)

# Vector of MODIS tiles to download
tiles <- get_tiles(rim)

rim_sp <- as(rim, 'Spatial')
plotKML(rim_sp["fire_name"], filename = "rim.kml")

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

# C6 ----------------------------------------------------------------------

rim_list <- list.files(file.path(yearly_events),
                        full.names = TRUE,
                       pattern = '_ms')

rim_rst <- stack(rim_list) %>%
  crop(as(rim_ms, 'Spatial')) %>%
  mask(as(rim_ms, 'Spatial')) 
rim_rst[rim_rst < 1] <- NA
rim_rst[rim_rst > max(rim_rst)+1] <- NA
plot(rim_rst[[13]])

rim_df6 <- as(rim_raw6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(august = ifelse(is.na(rim_C6_8), 0 , rim_C6_8),
         september = ifelse(is.na(rim_C6_9), 0 , rim_C6_9),
         october = ifelse(is.na(rim_C6_10), 0 , rim_C6_10),
         november = ifelse(is.na(rim_C6_11), 0 , rim_C6_11)) %>%
  mutate(value = pmax(august, september, october, november)) %>% 
  dplyr::select(value, x, y) 

library(viridis)  # better colors for everyone
rim_c6 <- ggplot() +  
  geom_tile(data=rim_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_polygon(data=as(rim, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('rim fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))
#  Cleaned and final plots ----------------------------------------------------------

rim_thresheld6 <- rim_raw6
rim_thresheld6[rim_thresheld6 < 226] <- NA
rim_thresheld6[rim_thresheld6 > 310] <- NA

rimT_df6 <- as(rim_thresheld6, "SpatialPixelsDataFrame")  %>%
  as.data.frame(.) %>%
  mutate(august = ifelse(is.na(rim_C6_8), 0 , rim_C6_8),
         september = ifelse(is.na(rim_C6_9), 0 , rim_C6_9),
         october = ifelse(is.na(rim_C6_10), 0 , rim_C6_10),
         november = ifelse(is.na(rim_C6_11), 0 , rim_C6_11)) %>%
  mutate(value = pmax(august, september, october, november)) %>% 
  dplyr::select(value, x, y) 


library(viridis)  # better colors for everyone
rim_c6 <- ggplot() +  
  geom_tile(data=rimT_df6, aes(x=x, y=y, fill=value), alpha=0.8) + 
  geom_contour(data=rimT_df6, aes(x=x, y=y, z=value), bins = 10, colour = 'black') +
  geom_polygon(data=as(rim, 'Spatial'), aes(x=long, y=lat, group=group), 
               fill=NA, color="black", size=1) +
  scale_fill_viridis('Burn Date', option = 'plasma', direction = -1) +
  ggtitle('rim fire: August 2012') +
  coord_equal() +
  theme_map() +
  theme(legend.position="bottom") +
  theme(legend.key.width=unit(7, "cm"))





library(gridExtra)
grid.arrange(rim_c5, rim_c6, nrow = 1)

