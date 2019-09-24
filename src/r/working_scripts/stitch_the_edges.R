# "aspatial" event creation
# author: Adam Mahood, May 2019
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

## install funique from github
remotes::install_github("mkearney/funique")

libs <- c("tidyverse", "raster", "foreach", "doParallel","sf","funique")
lapply(libs, library, character.only = TRUE, verbose = FALSE)


dir.create("data")

# getting the edge data frames
system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/edge_by_hem data/edges")


  
proj_modis <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
resolution <- 463.3127

sspace<-5
ttime<-11

ss <- sspace * resolution
tt <- ttime * resolution

#d<-read_csv("data/edges/wh_edges.csv")
#polygonizing
t0 <- Sys.time()
dd <- read_csv("data/edges/wh_edges.csv") %>%
  mutate(x = x + (resolution/2),
         y = y - (resolution/2)) %>%
  distinct(x,y,id, .keep_all = T) %>%
  st_as_sf(coords = c("x","y"),crs = proj_modis) %>%
  group_by(id) %>%
  # removing about 8000 repeat pixels
  # adding 1 m to the buffer to ensure things dissolve together nicely
  st_buffer(dist = 1+(resolution/2), endCapStyle = "SQUARE")%>%
  summarize(start_date = first(date),
            end_date = last(date))
  # then make buffer
print(Sys.time() - t0) # 8 min

# creating the buff # 1 min
t0 <- Sys.time()
ddb <- st_buffer(dd, dist = (ss/2)+1)
st_write(ddb, "data/wh_edge_buffers.gpkg", delete_dsn = TRUE)
print(Sys.time() - t0)

t0 <- Sys.time()
st_overlaps(ddb, sparse = TRUE) -> x # 5 min-ish
print(Sys.time() - t0)


dd$overlap <- NA
for(i in 1:nrow(dd)){
  dd[i,5] <- ifelse(length(x[[i]]) > 0, TRUE, FALSE)
}
st_write(dd, "data/wh_edge_polys.gpkg", delete_dsn = TRUE)
table(dd$overlap)

st_write(filter(dd, overlap == FALSE), "data/wh_edges_no_overlaps.gpkg", delete_dsn = TRUE)
st_write(filter(dd, overlap == TRUE), "data/wh_edges_yes_overlaps.gpkg", delete_dsn = TRUE)

ll <- vector(length = nrow(dd))
for(i in 1:nrow(x)){
  ll[i] <- length(x[[i]])
}
max(ll)

xx <- dd$overlap
for(i in 1:nrow(dd)){
  if(dd$overlap[i] == T){
    print("yay")
    rows <- x[[i]]
    rows1 <- c(rows, x[rows]) %>% unlist() %>% funique()
    while(length(rows1) != length(rows)){
      rows <- c(rows1, x[rows1]) %>% unlist() %>% funique()
      rows1 <- c(rows, x[rows]) %>% unlist() %>% funique()
    }
    
    dd_subset <- dd[rows1,]
    for(i in nrow(subset)){
      start_range <- dd_subset$start_date[i] - 11
      end_range <- dd_subset$end_date[i] + 11
      range <- start_range:end_range
      ww <- which(as.numeric(dd_subset$start_date) %in% range)
      ww <- c(ww, which(as.numeric(dd_subset$end_date) %in% range)) %>% funique
      if(length(ww) != 1){
        dd_subset[ww,]$id <- dd_subset[ww,]$id %>% min()
      }
    }
    
  }else{print("skip")}
}






# new_id <- 1

# trying to prepare a vectorized solution
ddrows <- nrow(dd)
t0 <- Sys.time()
vector_out <- rep(NA, ddrows)

ids <- funique(dd[,"id"])

for(i in 1:length(ids)){
  # if(is.na(vector_out[i])){
  print(paste(round(i/length(ids)*100, 3), "%"))
  
  event <- dd[dd[,"id"]==ids[i],]
  startrows <- nrow(event)

  if(!is.null(startrows)){
    id <- ids[i]
    vector_out[event[,"rownums"]] <- id
    
    
    maxd <- max(event[,"date"])+tt
    mind <- min(event[,"date"])-tt
    maxx <- max(event[,"x"])+ss
    minx <- min(event[,"x"])-ss
    maxy <- max(event[,"y"])+ss
    miny <- min(event[,"y"])-ss
    
    ddd <- dd[dd[,"date"] >=  mind & 
                dd[,"date"]<=  maxd &
                dd[,"x"] >=  minx& 
                dd[,"x"] <=  maxx &
                dd[,"y"]>=  miny & 
                dd[,"y"] <=  maxy, 
              ]
    endrows <- nrow(ddd)
    
    if(startrows != endrows){
      maxd <- max(event[,"date"])+100
      mind <- min(event[,"date"])-100
      maxx <- max(event[,"x"])+100000
      minx <- min(event[,"x"])-100000
      maxy <- max(event[,"y"])+100000
      miny <- min(event[,"y"])-100000
      
      ddd <- dd[dd[,"date"] >=  mind & 
                  dd[,"date"]<=  maxd &
                  dd[,"x"] >=  minx& 
                  dd[,"x"] <=  maxx &
                  dd[,"y"]>=  miny & 
                  dd[,"y"] <=  maxy, 
                ]
      endrows <- nrow(ddd)
    while(startrows != endrows){
      
      startrows <- sum(na.omit(vector_out == id))
      
      for(j in 1:nrow(event)){
      st_cube <- c(event[j, "x"] - ss, event[j, "x"] + ss,
                   event[j, "y"] - ss, event[j, "y"] + ss,
                   event[j, "date"] - tt, event[j, "date"] + tt)
    
      rs <- dd[dd[,"date"] >= st_cube[5] & 
               dd[,"date"]<= st_cube[6]  &
               dd[,"x"] >= st_cube[1] & 
               dd[,"x"] <= st_cube[2] &
               dd[,"y"]>= st_cube[3] & 
               dd[,"y"] <= st_cube[4], 
               "rownums"]
      # ude <- funique(vector_out[rs])
      # 
      #   if(length(ude) == 1){
      #     if(is.na(ude)){
      #       vector_out[rs] <- id
      #     }
      #   }else{
      #     vector_out[rs] <- ude %>%
      #       na.omit() %>%
      #       min()
      #   }
       vector_out[rs] <- id
      
      endrows <- sum(na.omit(vector_out == id))
      print(endrows)
      }
    }}
  }
}


print(Sys.time()-t0)
write_csv(dd, "data/r_events.csv")
system(paste0("aws s3 cp data/r_events.csv ",
              "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/event_dataframes/r_events_s",
              sspace, "_t" ,ttime, ".csv"))

# 
# crss <- crs(raster(tifs[1]), asText=T)
# subset <- filter(dd, burn_date<550) %>%
#   st_as_sf(coords = c("x", "y"),crs = crss)
#   
# st_write(subset, "subset.gpkg", delete_dsn = TRUE)
