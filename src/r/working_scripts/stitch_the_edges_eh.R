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
system("aws s3 sync s3://earthlab-natem/modis-burned-area/raw/world_boundaries data/wb")



proj_modis <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
resolution <- 463.3127

sspace<-5
ttime<-11

ss <- sspace * resolution
tt <- ttime

#d<-read_csv("data/edges/wh_edges.csv")
#first create points and overlay with continent

t0 <- Sys.time()
dd <- read_csv("data/edges/eh_edges.csv") %>%
  mutate(x = x + (resolution/2),
         y = y - (resolution/2)) %>%
  distinct(x,y,id, .keep_all = T) %>%
  st_as_sf(coords = c("x","y"),crs = proj_modis)
print(Sys.time()-t0)

st_write(dd,"data/eh_edge_points.gpkg")




dd1 <- dd %>%
  group_by(id) %>%
  # removing about 8000 repeat pixels
  # adding 1 m to the buffer to ensure things dissolve together nicely
  st_buffer(dist = 1+(resolution/2), endCapStyle = "SQUARE")%>%
  summarize(start_date = first(date),
            end_date = last(date))
# then make buffer
print(Sys.time() - t0) # 8 min wh

# creating the buff # 1 min wh
t0 <- Sys.time()
ddb <- st_buffer(dd, dist = (ss/2)+1)
st_write(ddb, "data/eh_edge_buffers.gpkg", delete_dsn = TRUE)
print(Sys.time() - t0)

t0 <- Sys.time()
st_overlaps(ddb, sparse = TRUE) -> x # 5 min-ish wh
print(Sys.time() - t0)


dd$overlap <- NA
for(i in 1:nrow(dd)){
  dd[i,5] <- ifelse(length(x[[i]]) > 0, TRUE, FALSE)
}
st_write(dd, "data/eh_edge_polys.gpkg", delete_dsn = TRUE)
# table(dd$overlap)

# st_write(filter(dd, overlap == FALSE), "data/wh_edges_no_overlaps.gpkg", delete_dsn = TRUE)
# st_write(filter(dd, overlap == TRUE), "data/wh_edges_yes_overlaps.gpkg", delete_dsn = TRUE)

# ll <- vector(length = nrow(dd))
# for(i in 1:nrow(x)){
#   ll[i] <- length(x[[i]])
# }
# max(ll)

t0 <- Sys.time()
res <- list()
counter <- 1
xx <- dd$overlap
for(i in 1:nrow(dd)){
  if(xx[i] == TRUE){
    print(paste(i,"doin' it", i/nrow(dd) * 100, "%"))
    rows <- x[[i]]
    rows1 <- c(rows, x[rows]) %>% unlist() %>% funique()
    while(length(rows1) != length(rows)){
      rows <- c(rows1, x[rows1]) %>% unlist() %>% funique()
      rows1 <- c(rows, x[rows]) %>% unlist() %>% funique()
    }
    
    dd_subset <- dd[rows1,]
    orig_ids <- dd_subset$id
    for(j in 1:nrow(dd_subset)){
      if(orig_ids[j] == dd_subset$id[j]){
        start_range <- dd_subset$start_date[j] - ttime
        end_range <- dd_subset$end_date[j] + ttime
        range <- start_range:end_range
        ww <- which(as.numeric(dd_subset$start_date) %in% range)
        ww <- c(ww, which(as.numeric(dd_subset$end_date) %in% range)) %>% funique
        if(length(ww) > 1){
          dd_subset[ww,]$id <- dd_subset[ww,]$id %>% min()
        }
      }
    }
    xx[rows1] <- FALSE
    res[[counter]] <- dd_subset
    counter <- counter + 1
  }else{print(paste(i,"skipin' it", i/nrow(dd) * 100, "%"))}
}

print(Sys.time()-t0)

done <- do.call('rbind', res) %>%
  group_by(id) %>%
  summarise(start_date = min(start_date),
            end_date = max(end_date))

st_write(done,"eh_edges_stitched.gpkg")
