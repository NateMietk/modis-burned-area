
# getting landcover
ll<-list()
cc <- 1
years <- 2002:2018
for(y in years){
  lc <- raster(file.path(local_data, paste0("MCD12Q1_mosaics/usa_lc_mosaic_",y-1,".tif")))
  ll[[cc]] <- df[df$year == y,] %>%
    st_as_sf(coords = c("x","y"), crs = crs(template, asText=TRUE))%>%
    mutate(lc = raster::extract(x=lc, y=.))
  cc<-cc+1
}

df_lc <- do.call("rbind", ll) %>%
  mutate(l1_eco = raster::extract(x=e_rast, y=.))

st_write(df_lc, "data/modis_events_w_landcover_01_18.gpkg")


labels <- st_set_geometry(ecoregions,NULL) %>%
  mutate(dup = duplicated(NA_L1CODE)) %>%
  filter(dup == FALSE) %>%
  dplyr::select(-dup) %>%
  dplyr::rename(l1_eco = NA_L1CODE)

lc_labels <- read_csv("data/usa_landcover_t1_classes.csv") %>%
  rename(lc = value, lc_name = name)

daily <- df_lc %>%
  st_set_geometry(NULL) %>%
  group_by(id, date) %>%
  summarise(pixels = n(),
            l1_eco = getmode(l1_eco),
            lc = getmode(lc)) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(cum_pixels = cumsum(pixels),
         total_pixels = sum(pixels),
         ignition_date = min(date),
         last_date = max(date),
         duration = as.numeric(last_date-ignition_date)+1,
         simple_fsr_pixels = total_pixels/as.numeric(duration), 
         simple_fsr_km2 = total_pixels/as.numeric(duration)* 463*463/1000000)%>%
  ungroup()%>%
  mutate(daily_area_km2 = pixels * 463*463/1000000,
         cum_area_km2 = cum_pixels * 463*463/1000000,
         total_area_km2 = total_pixels * 463*463/1000000,
         pct_total_area = pixels/total_pixels*100,
         pct_cum_area = pixels/cum_pixels*100,
         event_day = as.numeric(date - ignition_date + 1),
         ratio_area_added_to_average = daily_area_km2/simple_fsr_km2,
         prior_pixels = cum_pixels-pixels,
         rel_fsr_per_day = ifelse(prior_pixels>0,pixels/prior_pixels, 0) # need to decide on a value for that
  ) %>%
  left_join(labels) %>%
  left_join(lc_labels) %>%
  filter(is.na(l1_eco) == F)

write_csv(daily,"data/daily_06_10.csv")

event <- daily %>%
  group_by(id) %>%
  summarise(lc_name = getmode(lc_name),
            l1_ecoregion = getmode(l1_ecoregion),
            ignition_date = first(ignition_date),
            last_date = first(last_date),
            total_pixels = first(total_pixels),
            total_area_km2 = first(total_area_km2),
            duration = first(duration),
            simple_fsr_pixels = first(simple_fsr_pixels),
            simple_fsr_km2 = first(simple_fsr_km2)) %>%
  ungroup()

nrow(event)
nrow(df_poly)

# this takes a few minutes
ecoregions <- st_read("/home/a/data/background/ecoregions/us_eco_l1.gpkg") %>%
  mutate(l1_ecoregion = as.character(l1_ecoregion))

elabs <- ecoregions$l1_ecoregion %>% as.character()

table(daily$lc_name)
daily <- filter(daily, lc_name != "Permanent Snow and Ice",
                lc_name != "Water Bodies",
                lc_name != "Urban and Built-up Lands",
                lc_name != "Barren",
                lc_name != "Permanent Wetlands") %>%
  dplyr::mutate(lc_name = replace(lc_name, 
                                  lc_name == "Cropland/Natural  Vegetation  Mosaics",
                                  "Crops/Natural Veg"),
                lc_name = replace(lc_name, lc_name == "Woody Savannas", "Savannas"),
                lc_name = replace(lc_name, lc_name == "Closed Shrublands", "Shrublands"),
                lc_name = replace(lc_name, lc_name == "Open Shrublands", "Shrublands"),
                lc_name = replace(lc_name, lc_name == "Deciduous Broadleaf Forests", "Broadleaf Forests"),
                lc_name = replace(lc_name, lc_name == "Evergreen Broadleaf Forests", "Broadleaf Forests"),
                lc_name = replace(lc_name, lc_name == "Evergreen Needleleaf Forests", "Conifer Forests"))

values <- RColorBrewer::brewer.pal(length(unique(daily$lc_name)), "Dark2")
names(values) <- unique(daily$lc_name)

er <- list()
da <- list()
cp <- list()
labs <- vector()
for(i in ecoregions$NA_L1CODE){
  er[[i]] <- ggplot(ecoregions) + 
    geom_sf(data=ecoregions,fill = "transparent", color = "grey", size=0.1) +
    geom_sf(data = filter(ecoregions, NA_L1CODE == i), color = "black", size=0.3)+
    theme_void()+
    theme(panel.grid.major = element_line(color = "transparent"))
  
  da[[i]]<- ggplot(filter(daily, l1_eco == i), aes(x=event_day, cum_area_km2, 
                                                   group = id)) +
    geom_line(alpha=0.5, aes(color = lc_name)) +
    scale_color_manual(values = values)+
    theme_pubr() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none") +
    ylim(c(0,2620)) +
    xlim(c(0,213)) +
    ggtitle(filter(ecoregions, NA_L1CODE == i)$l1_ecoregion) +
    theme(plot.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  cp[[i]] <- ggdraw() +
    draw_plot(da[[i]]) +
    draw_plot(er[[i]], x = 0.55, y=0.5, width = 0.45, height=0.45)
  
  labs[i] <- filter(ecoregions, NA_L1CODE == i)$l1_ecoregion
}

leg <- get_legend(ggplot(daily, aes(x=event_day, cum_area_km2, 
                                    group = id)) +
                    geom_line(alpha=0.5, aes(color = lc_name)) +
                    scale_color_manual(values = values,name = "Land Cover")+
                    guides(color=guide_legend(ncol=2)))

#cp[[11]]<- as_ggplot(leg)

x=ggarrange(plotlist=cp, nrow=3, ncol=4) %>%
  annotate_figure(left = text_grob(expression(Area~Burned~(km^2)), rot = 90, size = 20),
                  bottom = text_grob("Days From Ignition", size = 20)) 

y=ggdraw() +
  draw_plot(x) +
  draw_plot(leg, x=0.6, y=0.1, width = 0.4, height = .25)+
  ggsave("images/ecoregions_cum_area.pdf", 
         dpi=600, width = 10, height = 7.5)
