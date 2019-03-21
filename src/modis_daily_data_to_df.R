library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(cowplot)
# library(gridExtra)
# library(foreach)
# library(doParallel)

#functions ---------------------------
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# do the business -----------

#corz <- detectCores()-1
scrapdir <- "scrap"
dir.create(scrapdir)
years = 2001:2017
crssss <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
# forgot how to escape quotes... just copy and paste these into the console for now
# aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_events/ /home/a/projects/modis-burned-area/scrap/ --recursive --exclude "*" --include "*events.tif"
# aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_composites/ /home/a/projects/modis-burned-area/scrap/ --recursive --exclude "*" --include "*ms.tif"

template <- Sys.glob(paste0("scrap/USA_BurnDate_",years[1],"*.tif"))[1] %>% raster
# template[is.na(template)==T] <- 0

ecoregions <- st_read("/home/a/data/background/ecoregions/us_eco_l3.shp") %>%
  mutate(NA_L1CODE = as.numeric(NA_L1CODE),
         l1_ecoregion = str_to_title(NA_L1NAME)) %>%
  st_transform(crs=crs(template, asText=TRUE)) %>%
  dplyr::select(NA_L1CODE, l1_ecoregion)

e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")
rm(template)

ll <- list()
for(y in 1:length(years)){
  filestobrick <- Sys.glob(paste0("scrap/USA_BurnDate*",years[y],"*.tif"))
  
  ll[[y]] <- raster::stack(filestobrick, e_rast) %>% 
  as.data.frame() %>%
  dplyr::select(event_id = paste0("USA_BurnDate_",years[y],"_events"),
                burn_doy = paste0("USA_BurnDate_",years[y]),
                NA_L1CODE = layer
                ) %>%
  filter(event_id > 0) %>%
  mutate(year = years[y],
         date = as.Date(paste(burn_doy, year, sep="-"), "%j-%Y"),
         modis_id = as.factor(paste("event", year, event_id, sep="_")))
  gc()
  print(years[y])
}
rm(e_rast)

gc()

df <- do.call("rbind", ll) %>%
  dplyr::select(-event_id) 

labels <- st_set_geometry(ecoregions,NULL) %>%
  mutate(dup = duplicated(NA_L1CODE)) %>%
  filter(dup == FALSE) %>%
  dplyr::select(-dup)

daily <- df %>%
  group_by(modis_id, date) %>%
  summarise(pixels = n(),
            NA_L1CODE = getmode(NA_L1CODE)) %>%
  ungroup() %>%
  group_by(modis_id) %>%
  mutate(cum_pixels = cumsum(pixels),
         total_pixels = sum(pixels),
         ignition_date = min(date),
         last_date = max(date),
         duration = last_date-ignition_date,
         simple_fsr_pixels = total_pixels/as.numeric(duration), 
         simple_fsr_km2 = total_pixels/as.numeric(duration)* 463*463/1000000)%>%
  ungroup()%>%
  mutate(daily_area_km2 = pixels * 463*463/1000000,
         cum_area_km2 = cum_pixels * 463*463/1000000,
         total_area_km2 = total_pixels * 463*463/1000000,
         pct_total_area = pixels/total_pixels*100,
         pct_cum_area = pixels/cum_pixels*100,
         event_day = as.numeric(date - ignition_date),
         ratio_area_added_to_average = daily_area_km2/simple_fsr_km2,
         prior_pixels = cum_pixels-pixels,
         rel_fsr_per_day = ifelse(prior_pixels>0,pixels/prior_pixels, NA) # need to decide on a value for that
         #rel_fsr_per_total = r
         ) %>%
  left_join(labels) %>%
  filter(is.na(l1_ecoregion) == F)

# map for insets -------------------
template <- raster("data/USA_BurnDate_2001.tif")

# this takes a few minutes
ecoregions <- st_read("/home/a/data/background/ecoregions/us_eco_l1.gpkg") 



er <- list()
da <- list()
cp <- list()
for(i in unique(ecoregions$NA_L1CODE)){
  er[[i]] <- ggplot(ecoregions) + 
    geom_sf(data=ecoregions,fill = "transparent", color = "grey", size=0.1) +
    geom_sf(data = filter(ecoregions, NA_L1CODE == i), color = "black", size=0.3)+
    theme_void()+
    theme(panel.grid.major = element_line(color = "transparent"))
  
  da[[i]]<- ggplot(filter(daily, NA_L1CODE == i), aes(x=event_day, cum_area_km2, group = modis_id)) +
    geom_line(alpha=0.5, aes(color = as.numeric(substr(as.character(date),1,4)))) +
    scale_color_gradient(high = "darkblue", low="orange", name = "Year")+
    theme_pubr() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none") +
    ylim(c(0,2070)) +
    xlim(c(0,108)) +
    ggtitle(ecoregions$l1_ecoregion[i])
  
  cp[[i]] <- ggdraw() +
    draw_plot(er[[i]], x = 0.6, y=0.6, width = 0.4, height=0.4)+
    draw_plot(da[[i]]) 
}

x=ggarrange(plotlist=cp, nrow=4, ncol=3,
          labels = ecoregions$l1_ecoregion) + 
  ggsave("images/ecoregions_cum_area.pdf", 
         dpi=600, width = 7.5, height =10)


# e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")

p1<- ggplot(daily, aes(x=event_day, cum_area_km2, group = modis_id)) +
  geom_line(alpha=0.5, aes(color = as.numeric(substr(as.character(date),1,4)))) +
  scale_color_gradient(high = "darkblue", low="orange", name = "Year")+
  facet_wrap(~l1_ecoregion) +
  theme_bw() +
  ggsave("images/cumulative_area.pdf", dpi = 600)


write_csv(daily, "data/daily_asof_20180320.csv")

matches <- st_read("data/matches_209_data.gpkg") %>%
  dplyr::select(INCIDENT_ID, modis_id) %>%
  st_set_geometry(NULL) %>%
  left_join(daily)

# only works for adam's computer... need to put these on s3 at some point
drive_find(pattern="ics209_allWFsitreps1999to2014.csv",n_max = 5) %>% 
  drive_download(path = "data/ics209_allWFsitreps1999to2014.csv", overwrite = TRUE)
drive_find(pattern="ics209_allWFincidents1999to2014.csv",n_max = 5) %>%
  drive_download(path = "data/ics209_allWFincidents1999to2014.csv", overwrite = TRUE)

sr_file <- "data/ics209_allWFsitreps1999to2014.csv"
sr <- read_csv(sr_file, guess_max = 500000) %>%
  mutate(date = as.Date(paste(REPORT_DOY, CY,sep="-"), "%j-%Y")) %>%
  left_join(matches, by = c("date", "INCIDENT_ID"))

ggplot(sr, aes(date, daily_area_km2, group=INCIDENT_ID)) +
  geom_line()

ggplot(daily, aes(x=event_day, daily_area_km2, group = modis_id)) +
  geom_line()
ggplot(daily, aes(x=event_day, cum_area_km2, group = modis_id)) +
  geom_line(alpha=0.5)
ggplot(daily, aes(x=event_day, rel_fsr_per_day, group = modis_id)) +
  geom_line()
# color lines by ecoregion, primaty fuel type (nlcd or something)
# ecoregion
# case study figure
# 
# exploration ------
dd <- filter(daily, duration == max(duration))
ggplot(dd, aes(x=as.numeric(date), y=total_pixels)) +
  geom_line(aes(y=cum_pixels)) +
  geom_segment(y=0,yend=1180, 
               x=as.numeric(dd$ignition_date[1]),xend=as.numeric(dd$last_date[1]))

            