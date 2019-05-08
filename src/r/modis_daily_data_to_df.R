# functions --------------------------------------------------------------------
iini <-function(x){
  #stands for install if not installed
  if (!x %in% rownames(installed.packages())) install.packages(x)
}

segment_dat <- function(dd, res){
  mod1 <- lm(cum_pixels~event_day, dd)
  #polymod <-lm(poly(cum_pixels,2)~event_day, dd)
  segmod <- segmented(mod1, 
                      seg.Z = ~ event_day, 
                      psi = c(2))
  dd$fitted = fitted(segmod)
  
  segs <- data.frame(day_break = c(as.numeric(segmod$psi[,2]),
                                   as.numeric(segmod$rangeZ[2,1]))) %>%
    round()%>%
    mutate(pixel_break = dd$fitted[dd$event_day %in% .$day_break])%>%
    mutate(pixel_lag = lag(pixel_break, default = 0),
           pixel_gain = pixel_break-pixel_lag,
           day_lag = lag(day_break, default=0),
           day_gain = day_break - day_lag,
           slope = pixel_gain/day_gain)
  
  peak_growth_pixels <- segs[segs$pixel_gain == max(segs$pixel_gain),]$pixel_gain
  peak_growth_days <- segs[segs$pixel_gain == max(segs$pixel_gain),]$day_gain
  peak_growth_fsr <- segs[segs$pixel_gain == max(segs$pixel_gain),]$slope
  
  if(res == "pix") return(peak_growth_pixels)
  if(res == "days") return(peak_growth_days)
  if(res == "fsr") return(peak_growth_fsr)
  
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
# setup ------------------------------------------------------------------------
libs <- c("tidyverse", "raster", "sf","fasterize", "cowplot", "ggpubr", 
          "segmented")

lapply(libs, iini)
lapply(libs, library, character.only = TRUE, verbose = FALSE)


# do the business -----------

#corz <- detectCores()-1
scrapdir <- "scrap"
dir.create(scrapdir, showWarnings = FALSE)
years = 2001:2017
crssss <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
# forgot how to escape quotes... just copy and paste these into the console for now
# aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_events/ /home/a/projects/modis-burned-area/scrap/ --recursive --exclude "*" --include "*events_ms.tif"
# aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_composites/ /home/a/projects/modis-burned-area/scrap/ --recursive --exclude "*" --include "*ms.tif"

template <- Sys.glob(paste0("scrap/USA_BurnDate_",years[1],"*.tif"))[2] %>% raster
# template[is.na(template)==T] <- 0

ecoregions <- st_read("/home/a/data/background/ecoregions/us_eco_l3.shp") %>%
  mutate(NA_L1CODE = as.numeric(NA_L1CODE),
         l1_ecoregion = str_to_title(NA_L1NAME)) %>%
  st_transform(crs=crs(template, asText=TRUE)) %>%
  dplyr::select(NA_L1CODE, l1_ecoregion)

e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")

lc <- raster("data/usa_landcover_t1_2017.tif") %>%
  resample(template)

ll <- list()
for(y in 1:length(years)){
  filestobrick <- Sys.glob(paste0("scrap/USA_BurnDate*",years[y],"*.tif"))
  r2 <- raster(filestobrick[2]) 
  r1 <- raster(filestobrick[1])%>%
    resample(r2)

  ll[[y]] <- raster::stack(r1,r2, e_rast, lc) %>% 
  as.data.frame() %>%
  dplyr::select(event_id = paste0("USA_BurnDate_",years[y],"_events_ms"),
                burn_doy = paste0("USA_BurnDate_",years[y],"_ms"),
                NA_L1CODE = layer,
                landcover = usa_landcover_t1_2017
                ) %>%
  filter(event_id > 0) %>%
  mutate(year = years[y],
         date = as.Date(paste(burn_doy, year, sep="-"), "%j-%Y"),
         modis_id = as.factor(paste("event", year, event_id, sep="_")))
  gc()
  print(years[y])
}

gc()

df <- do.call("rbind", ll) %>%
  dplyr::select(-event_id) 

labels <- st_set_geometry(ecoregions,NULL) %>%
  mutate(dup = duplicated(NA_L1CODE)) %>%
  filter(dup == FALSE) %>%
  dplyr::select(-dup)

lc_labels <- read_csv("data/usa_landcover_t1_classes.csv") %>%
  rename(landcover = value, landcover_name = name)

daily <- df %>%
  group_by(modis_id, date) %>%
  summarise(pixels = n(),
            NA_L1CODE = getmode(NA_L1CODE),
            landcover = getmode(landcover)) %>%
  ungroup() %>%
  group_by(modis_id) %>%
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
         event_day = as.numeric(date - ignition_date),
         ratio_area_added_to_average = daily_area_km2/simple_fsr_km2,
         prior_pixels = cum_pixels-pixels,
         rel_fsr_per_day = ifelse(prior_pixels>0,pixels/prior_pixels, NA) # need to decide on a value for that
         #rel_fsr_per_total = r
         ) %>%
  left_join(labels) %>%
  left_join(lc_labels) %>%
  filter(is.na(l1_ecoregion) == F)

daily_1 <- daily %>%
  filter(duration >0)%>%
  group_by(modis_id) %>%
  mutate(peak_growth_pixels = segment_dat(dd=., res = "pix"),
         peak_growth_days = segment_dat(dd=., res = "days"),
         peak_growth_fsr = segment_dat(dd=., res = "fsr"))

for(i in unique(daily$modis_id)){
  x <- filter(daily, modis_id == i) %>%
    dplyr::select(cum_pixels, event_day, modis_id, duration)
  if(x$duration > 40 ){
    y<-  segment_dat(dd=x, res = "pix")
    print(y)
  }
}

#write_csv(daily, "data/daily_asof_20180321.csv")

daily <- read_csv("data/daily_asof_20180320.csv")

# map for insets -------------------
template <- raster("data/USA_BurnDate_2001.tif")

# this takes a few minutes
ecoregions <- st_read("/home/a/data/background/ecoregions/us_eco_l1.gpkg") %>%
  mutate(l1_ecoregion = as.character(l1_ecoregion))

elabs <- ecoregions$l1_ecoregion %>% as.character()

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
  
  da[[i]]<- ggplot(filter(daily, NA_L1CODE == i), aes(x=event_day, cum_area_km2, group = modis_id)) +
    geom_line(alpha=0.5, aes(color = as.numeric(substr(as.character(date),1,4)))) +
    scale_color_gradient(high = "darkblue", low="orange", name = "Year")+
    theme_pubr() +
    xlab("") +
    ylab("") +
    theme(legend.position = "none") +
    ylim(c(0,2070)) +
    xlim(c(0,108)) +
    ggtitle(filter(ecoregions, NA_L1CODE == i)$l1_ecoregion) +
    theme(plot.title = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  cp[[i]] <- ggdraw() +
    draw_plot(da[[i]]) +
    draw_plot(er[[i]], x = 0.55, y=0.5, width = 0.45, height=0.45)
  
  labs[i] <- filter(ecoregions, NA_L1CODE == i)$l1_ecoregion
}


x=ggarrange(plotlist=cp, nrow=3, ncol=4) %>%
  annotate_figure(left = text_grob(expression(Area~Burned~(km^2)), rot = 90, size = 20),
                  bottom = text_grob("Days From Ignition", size = 20)) +
    ggsave("images/ecoregions_cum_area.pdf", 
         dpi=600, width = 10, height = 7.5)


# e_rast <- fasterize(ecoregions, template, field="NA_L1CODE")

p1<- ggplot(daily, aes(x=event_day, cum_area_km2, group = modis_id)) +
  geom_line(alpha=0.5, aes(color = as.numeric(substr(as.character(date),1,4)))) +
  scale_color_gradient(high = "darkblue", low="orange", name = "Year")+
  facet_wrap(~l1_ecoregion) +
  theme_bw() +
  ggsave("images/cumulative_area.pdf", dpi = 600)



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


segment_dat <- function(dd, res){
  mod1 <- lm(cum_pixels~event_day, dd)

  if(summary(mod1)$r.squared < 80){
    segmod <- segmented(mod1, 
                        seg.Z = ~ event_day, 
                        psi = c(2,4,6))
    dd$fitted = fitted(segmod)
    
    segs <- data.frame(day_break = c(as.numeric(segmod$psi[,2]),
                                     as.numeric(segmod$rangeZ[2,1]))) %>%
      round()%>%
      mutate(pixel_break = dd$fitted[dd$event_day %in% .$day_break])%>%
      mutate(pixel_lag = lag(pixel_break, default = 0),
             pixel_gain = pixel_break-pixel_lag,
             day_lag = lag(day_break, default=0),
             day_gain = day_break - day_lag,
             slope = pixel_gain/day_gain)
    
    peak_growth_pixels <- segs[segs$pixel_gain == max(segs$pixel_gain),]$pixel_gain
    peak_growth_days <- segs[segs$pixel_gain == max(segs$pixel_gain),]$day_gain
    peak_growth_fsr <- segs[segs$pixel_gain == max(segs$pixel_gain),]$slope
  }
  
  if(res == "pix") return(peak_growth_pixels)
  if(res == "days") return(peak_growth_days)
  if(res == "fsr") return(peak_growth_fsr)

}

segment_dat(dd=dd, res="fsr")

ggplot(dd, aes(x=event_day, y=total_pixels)) +
  geom_line(aes(y=cum_pixels)) +
  geom_line(aes(y=predict(mod1)), color = "red")+
  geom_vline(xintercept = segmod$psi[,2], lty=2)+
# geom_line(aes(y=predict(polymod)), color = "green")+
  geom_line(aes(y=predict(segmod)), color = "blue") #+
  #geom_segment(y=0,yend=1180, 
           #    x=as.numeric(dd$ignition_date[1]),xend=as.numeric(dd$last_date[1]))

            