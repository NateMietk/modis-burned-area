# aspatial event creation
libs <- c("tidyverse", "raster", "foreach", "doParallel","sf")
lapply(libs, library, character.only = TRUE, verbose = FALSE)
dir.create("data")
dir.create("data/scrap")
dir.create("data/scrap/tif_alltiles")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/tif_converted_alltiles/ /home/a/projects/modis-burned-area/data/scrap/tif_alltiles/")

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

dd$event <- NA
# xrange <- unique(dd$x) %>% sort
# yrange <- unique(dd$y) %>% sort
#zrange <- unique(dd$burn_date) %>% sort

ss <- 5
tt <- 11
new_id <- 1

# slow way ------------------------------
# t0 <- Sys.time()
# for (zz in zrange){
#   print(zz)
#   xrange<-unique(dd[dd$burn_date < zz + 30 & dd$burn_date > zz - 30,])$x
#    
#  #xrange <- seq(min(xr), max(xr))
#   
#   for (xx in xrange){
#     yrange<-unique(dd[dd$burn_date < zz + 30 & dd$burn_date > zz - 30 & dd$x < xx+30 & dd$x > xx-30,])$y
#     #yrange <- seq(min(yr), max(yr))
#     #print(xx)
#     for (yy in yrange){
#     
#       st_cube_x <- seq(xx, xx + ss*463, by = 463)
#       st_cube_y <- seq(yy, yy + ss*463, by = 463)
#       st_cube_z <- seq(zz, zz + tt)
#       
#       data_slice <- dd %>%
#         filter(burn_date > min(st_cube_z) - 1 & burn_date < max(st_cube_z) + 1 &
#         x > min(st_cube_x) - 1 & x < max(st_cube_x) + 1 &
#         y > min(st_cube_y) - 1 & y < max(st_cube_y) + 1)
#       
#       
#       if(nrow(data_slice)>0){ print(nrow(data_slice))}
#       
#       if(nrow(data_slice) > 0) {
#         if(length(unique(data_slice$event)) == 1){
#           if(is.na(unique(data_slice$event))){
#             dd[dd$x %in% data_slice$x & 
#                  dd$y %in% data_slice$y  &
#                  dd$burn_date %in% data_slice$burn_date,]$event <- new_id
#             new_id <- new_id+1
#           }else{
#             dd[dd$x %in% data_slice$x & 
#                  dd$y %in% data_slice$y  &
#                  dd$burn_date %in% data_slice$burn_date,]$event <- unique(data_slice$event)
#             
#           } # end of else (when the the one value is an event)
#         }else{
#           lowest_id <- unique(data_slice$event) %>%
#             na.omit() %>%
#             min()
#           dd[dd$x %in% data_slice$x & 
#                dd$y %in% data_slice$y  &
#                dd$burn_date %in% data_slice$burn_date,]$event <- lowest_id
#           
#         } # end of else (when rows are > 1)
#       } # end of if(data_slice>0)
#     }# end of xyz for loops
#   }
# }
# print(Sys.time()-t0)
# write_csv(dd, "data/r_events.csv")
# system("aws s3 cp /home/a/projects/modis-burned-area/data/r_events.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/r_events.csv")

# better way -----------------------------------------
new_id <- 1
dd$event <- NA
t0 <- Sys.time()
for(i in 1:nrow(dd)){
  if(is.na(dd$event[i])){
    print(paste(i, round(i/nrow(dd)*100, 3), "%"))
    st_cube_x <- seq(dd$x[i] - (ss*463.3127), dd$x[i] + (ss*463.3127), by = 463)
    st_cube_y <- seq(dd$y[i] - (ss*463.3127), dd$y[i] + (ss*463.3127), by = 463)
    st_cube_z <- seq(dd$burn_date[i]-tt, dd$burn_date[i] + tt)
    
    data_slice <- dd %>%
      filter(burn_date > min(st_cube_z) - 1 & burn_date < max(st_cube_z) + 1 &
               x > min(st_cube_x) - 1 & x < max(st_cube_x) + 1 &
               y > min(st_cube_y) - 1 & y < max(st_cube_y) + 1)
    
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
    
    # if(nrow(data_slice) > 0) {
    #   if(length(unique(data_slice$event)) == 1){
    #     if(is.na(unique(data_slice$event))){
    #       dd[dd$x %in% data_slice$x & 
    #            dd$y %in% data_slice$y  &
    #            dd$burn_date %in% data_slice$burn_date,]$event <- new_id
    #       new_id <- new_id+1
    #     }else{
    #       dd[dd$x %in% data_slice$x & 
    #            dd$y %in% data_slice$y  &
    #            dd$burn_date %in% data_slice$burn_date,]$event <- unique(data_slice$event)
    #       
    #     } # end of else (when the the one value is an event)
    #   }else{
    #     lowest_id <- unique(data_slice$event) %>%
    #       na.omit() %>%
    #       min()
    #     dd[dd$x %in% data_slice$x & 
    #          dd$y %in% data_slice$y  &
    #          dd$burn_date %in% data_slice$burn_date,]$event <- lowest_id
    #   }
    # }
  }
}
print(Sys.time()-t0)
write_csv(dd, "data/r_events.csv")
system("aws s3 cp /home/a/projects/modis-burned-area/data/r_events.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/r_events.csv")


crss <- crs(raster(tifs[1]), asText=T)
subset <- filter(dd, burn_date<550) %>%
  st_as_sf(coords = c("x", "y"),crs = crss)
  
st_write(subset, "subset.gpkg", delete_dsn = TRUE)
