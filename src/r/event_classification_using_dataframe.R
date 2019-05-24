# "aspatial" event creation
# author: Adam Mahood, May 2019

libs <- c("tidyverse", "raster", "foreach", "doParallel","sf")
lapply(libs, library, character.only = TRUE, verbose = FALSE)

dir.create("data")
dir.create("data/scrap")
dir.create("data/scrap/tif_alltiles")

system("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/bd_no_events.csv data/bd_no_events.csv")


wd <- getwd()

system(paste("aws s3 sync",
             "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/tif_converted_alltiles/"
             ,file.path(wd, "data/scrap/tif_alltiles/")))

tifs <- list.files("data/scrap/tif_alltiles", full.names = T) 

#tifs <- "example.tif"

corz <- detectCores()-1
registerDoParallel(corz)

t0 <- Sys.time()
dd <- foreach(i = 1:length(tifs), .combine = 'rbind') %dopar% {
  
  system(paste("echo", round(i/length(tifs),3)*100, "%"))
  
  rr <- raster(tifs[i])
  names(rr) <- "burn_date"
  rr<- rr %>%
    as.data.frame(xy=TRUE, na.rm=T)
  
  return(rr)
}
print(Sys.time()-t0) # 15 minutes on adam's computer
t1 <- Sys.time()

write_csv(dd, "data/bd_no_events.csv")
system("aws s3 cp data/bd_no_events.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/bd_no_events.csv")

dd <- read_csv("data/bd_no_events.csv")

resolution<- 463.3127

ss <- 5/2*resolution
tt <- 11/2 * resolution
new_id <- 1
dd$event <- NA

# condition1 <- nrow(data_slice) > 0
# condition2 <-length(unique(data_slice$event)) == 1
# condition3 <- is.na(unique(data_slice$event))

t0 <- Sys.time()
for(i in 1:nrow(dd)){
  if(is.na(dd$event[i])){
    print(paste(i, round(i/nrow(dd)*100, 3), "%"))
    st_cube_x <- c(dd$x[i] - ss, dd$x[i] + ss)
    st_cube_y <- c(dd$y[i] - ss, dd$y[i] + ss)
    st_cube_z <- c(dd$burn_date[i]-tt, dd$burn_date[i] + tt)
    
    data_slice <- dd %>%
      filter(burn_date > st_cube_z[1] - 1 & burn_date < st_cube_z[2] + 1 &
               x > st_cube_x[1] - 1 & x < st_cube_x[2] + 1 &
               y > st_cube_y[1] - 1 & y < st_cube_y[2] + 1)
    
    if(nrow(data_slice) > 0) {
      if(length(unique(data_slice$event)) == 1){
        if(is.na(unique(data_slice$event))){
          dd[dd$x %in% data_slice$x & 
               dd$y %in% data_slice$y  &
               dd$burn_date %in% data_slice$burn_date,]$event <- new_id
          new_id <- new_id+1
        }else{
          dd[dd$x %in% data_slice$x & 
               dd$y %in% data_slice$y  &
               dd$burn_date %in% data_slice$burn_date,]$event <- unique(data_slice$event)
          
        } # end of else (when the the one value is an event)
      }else{
        lowest_id <- unique(data_slice$event) %>%
          na.omit() %>%
          min()
        dd[dd$x %in% data_slice$x & 
             dd$y %in% data_slice$y  &
             dd$burn_date %in% data_slice$burn_date,]$event <- lowest_id
        
      } # end of else (when rows are > 1)
    } # end of if(data_slice>0)
   
  }
}
print(Sys.time()-t0)
write_csv(dd, "data/r_events.csv")
system("aws s3 cp /home/a/projects/modis-burned-area/data/r_events.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/r_events_s5d_t11d.csv")


crss <- crs(raster(tifs[1]), asText=T)
subset <- filter(dd, burn_date<550) %>%
  st_as_sf(coords = c("x", "y"),crs = crss)
  
st_write(subset, "subset.gpkg", delete_dsn = TRUE)
