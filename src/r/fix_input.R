library(rebus)
library(tidyverse)
library(sf)
library(doParallel)
library(foreach)
library(raster)

#prep ================
tiles_w_fire <- system(" aws s3 ls s3://earthlab-natem/modis-burned-area/delineated_events/world/events --recursive
                       ", intern = TRUE) %>%
  str_match(pattern = capture(one_or_more(WRD)) %R% ".csv" %R% END) %>%
  na.omit() 
csvs <- tiles_w_fire[,1]
twf <- tiles_w_fire[,2]

# paste0(tiles_w_fire[,2], collapse= " ")

# borders <- st_read("~/data/background/world_borders/ne_50m_admin_0_countries.shp") %>%
#   st_transform(crs = st_crs(modis_tiles))
template_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

reso <- 463.3127
pix_hectares <- function(p) p * 463.3127*463.3127 * 0.0001

# the biz ===========
dir.create("data/scrap", recursive = TRUE)
dir.create("data/scrap/edge")
dir.create("data/scrap/notedge")

corz <- detectCores()-1
registerDoParallel(corz)
foreach(c = twf, .combine = rbind)%dopar%{
  #prep
  csvfile <- paste0("data/scrap/",c, ".csv")
  outputn <- paste0("data/scrap/","noedge_",c, ".gpkg")
  outpute <- paste0("data/scrap/","edge_",c, ".csv")
  
  tilenum <- paste0("1",substr(c, 2,3), substr(c,5,6),"0000000") %>% as.numeric
  system(paste0("aws s3 cp ",
                       "s3://earthlab-natem/modis-burned-area/delineated_events/",
                       "world/events/", c, ".csv ",
                       csvfile))
  ne <- read_csv(csvfile) %>%
    filter(edge == TRUE) %>%
    nrow()
  
  #the bizness
  n <- read_csv(csvfile) %>%
    dplyr::filter(edge == FALSE) %>%
    dplyr::select(-duration, -tile, -edge) %>%
    mutate(x = x + (reso/2),
           y = y - (reso/2),
           year = as.numeric(substr(date, 1,4)),
           id = id+tilenum) %>%
    distinct(x,y,id, .keep_all = T) %>%
    st_as_sf(coords = c("x","y"), crs = template_crs) %>%
    group_by(id)%>%
    st_buffer(dist = 1+(reso/2), endCapStyle = "SQUARE")%>%
    summarize(start_date = first(date),
              last_date = last(date),
              area_burned_ha = pix_hectares(n())) %>%
    ungroup()
  st_write(n, outputn, delete_dsn = TRUE)
  system(paste0("aws s3 cp ", outputn," ",
               "s3://earthlab-natem/modis-burned-area/",
               "delineated_events/world/noedge/", "noedge_",c,".gpkg"))
  
 
  
  if(ne>0){
    e <- read_csv(csvfile) %>%
      dplyr::filter(edge == TRUE)%>%
      dplyr::select(-duration, -tile, -edge)
    
    write_csv(e, outpute)
    system(paste0("aws s3 cp ", outpute," ",
                  "s3://earthlab-natem/modis-burned-area/",
                  "delineated_events/world/edge/", "edge_",c,".csv"))}  
  unlink(csvfile)
}
