# spatial join 209 data to modis
library(tidyverse)
library(raster)
library(sf)
library(foreach)
library(doParallel)

scrapdir <- "scrap"
dir.create(scrapdir)
events_table_filename <- "ics209_allWFincidents1999to2014.csv"
latlong<- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" 

events_209 <- read_csv(events_table_filename, guess_max=35000) 

s3_path<- "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_events"

cc<-detectCores()-1
registerDoParallel(cc)

years <- 2001:2014
foreach(i = 1:length(years))%dopar%{
  events_raster_filename <- file.path(paste0("USA_BurnDate_",years[i],"_events.tif"))
  system(paste("aws s3 cp", 
               file.path(s3_path, events_raster_filename),
               file.path("data", events_raster_filename)
         ))
  
  events_modis_r <- raster(file.path("data",events_raster_filename))
  

  events_209 <- read_csv(events_table_filename)%>%
    filter(is.na(POO_LATITUDE)==FALSE,
           START_YEAR==years[i])
  
  before <- nrow(events_209)
  
  events_209 <- events_209  %>%
    st_as_sf(coords=c("POO_LONGITUDE","POO_LATITUDE"), crs = latlong) %>%
    st_transform(crs=crs(events_modis_r, asText=T)) %>%
    st_buffer(2000) %>%
    mutate(modis_id = raster::extract(events_modis_r, ., fun = max)) %>%
    filter(modis_id>0) %>%
    mutate(modis_id = paste("event", years[i], modis_id, sep="_"))
  
  st_write(events_209, file.path(scrapdir, paste0("matches", years[i], ".gpkg")),
           delete_dsn = TRUE)
  
}


result_gpkgs <- list.files(scrapdir, full.names = T)



ll <- list()
results_df <- data.frame(year = NA, total_209s = NA, matches_w_mtbs=NA, pct_match = NA)
for(i in 1:length(result_gpkgs)){
  system(paste("aws s3 cp",
                       file.path(s3_path, paste0("USA_BurnDate_",years[i],"_stats.csv")),
                       file.path(scrapdir, paste0("USA_BurnDate_",years[i],"_stats.csv"))))
  stat_table <- read_csv(file.path(scrapdir, paste0("USA_BurnDate_",years[i],"_stats.csv"))) %>%
    mutate(modis_id = paste("event", years[i], eventID, sep="_"))
  ll[[i]] <- st_read(result_gpkgs[i]) %>%
    left_join(stat_table, by = "modis_id")
  results_df[i,1] <- years[i]
  results_df[i,2] <- read_csv(events_table_filename)%>%
    filter(is.na(POO_LATITUDE)==FALSE,
           START_YEAR==years[i]) %>%
    nrow
  
  results_df[i,3] <- nrow(ll[[i]])
  results_df[i,4] <- nrow(ll[[i]])/results_df[i,2]
}

st_write(do.call("rbind", ll), "matches_209_data.gpkg", delete_dsn = TRUE)
write.csv(results_df, "matches_209_modis_w_2000m_buffer.csv")

system(paste("aws s3 cp", "matches_209_modis_w_2000m_buffer.csv",
             "s3://earthlab-natem/modis-burned-area/matches_209_modis_w_2000m_buffer.csv"))
system(paste("aws s3 cp", "matches_209_data.gpkg",
             "s3://earthlab-natem/modis-burned-area/matches_209_data.gpkg"))

# messing around with analysis -------------------------------------------------

# download from s3
system(paste("aws s3 cp",
             "s3://earthlab-natem/modis-burned-area/matches_209_data.gpkg", "matches_209_data.gpkg"))
dd <- do.call("rbind", ll)
dd <- st_read("matches_209_data.gpkg") %>%
  filter(is.na(FINAL_ACRES)==F)

ggplot(dd, aes(x=area_km2, y=FINAL_ACRES)) + geom_point()

ddd <- dd %>%
  mutate(residual = resid(lm(area_km2 ~ FINAL_ACRES, .))) %>%
  filter(residual<sd(residual)*2) #%>%
  #filter(area_km2>250)
  
ggplot(ddd, aes(x=area_km2, y=FINAL_ACRES)) + geom_point()

ggplot(ddd, aes(x=fsr, y=WF_MAX_FSR)) + geom_point()
ggplot(ddd, aes(x=fsr, y=INJURIES_TOTAL)) + geom_point()
ggplot(ddd, aes(x=fsr, y=PROJECTED_FINAL_IM_COST)) + geom_point()
ggplot(ddd, aes(y=fsr, x=EVACUATION_REPORTED)) + geom_boxplot()
ggplot(ddd, aes(y=fsr, x=SUPPRESSION_METHOD)) + geom_boxplot()
ggplot(ddd, aes(y=fsr, x=CAUSE)) + geom_boxplot()
ggplot(ddd, aes(x=fsr, y=WF_PEAK_AERIAL)) + geom_point()
ggplot(ddd, aes(x=fsr, y=TOTAL_PERSONNEL_SUM)) + geom_point()
ggplot(ddd, aes(x=fsr, y=TOTAL_AERIAL_SUM)) + geom_point()
ggplot(ddd, aes(x=fsr, y=STR_THREATENED_MAX)) + geom_point()
ggplot(ddd, aes(x=fsr, y=STR_DESTROYED_TOTAL)) + geom_point()
ggplot(ddd, aes(x=fsr, y=STR_DAMAGED_TOTAL)) + geom_point()
