# edge stitching
# author: Adam Mahood, Sept 2019

#env prep ===============================================================================================
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
## install funique from github
remotes::install_github("mkearney/funique")

libs <- c("tidyverse", "raster", "foreach", "doParallel","sf","funique")
lapply(libs, library, character.only = TRUE, verbose = FALSE)


dir.create("data")

# getting the edge data frames
system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/edge data/edge_tiles")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/input/raw/world_boundaries data/wb")

proj_modis <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
resolution <- 463.3127

sspace<-5
ttime<-11

ss <- sspace * resolution
tt <- ttime

#d<-read_csv("data/edges/wh_edges.csv")
#first create points and overlay with continent

dir.create("data/wb_extracts")
wb <- st_read("data/wb") %>%
  dplyr::select(name = NAME,
                continent = CONTINENT) %>%
  mutate(cont_num = as.numeric(continent),
         country_num = as.numeric(name)) %>%
  dplyr::select(cont_num, country_num) %>%
  st_transform(crs=st_crs(proj_modis))

edge_tile_files <- list.files("data/edge_tiles", pattern = "csv", full.names = TRUE)[57:169]

tiles_done <- list.files("data/wb_extracts/") %>% str_extract("h\\d{2}v\\d{2}")

for(i in 1:length(edge_tile_files)){
  tile <- edge_tile_files[i] %>% str_extract("h\\d{2}v\\d{2}")
  
  if(!tile %in% tiles_done){
    system(paste("echo", tile))
    
    
    tilenum <- paste0("1",str_sub(tile, 2,3), str_sub(tile,5,6),"0000000") %>% as.numeric
    
    etd <- read_csv(edge_tile_files[i]) 
    uids <- funique(etd$id)
    
    corz <- detectCores()-1
    registerDoParallel(corz)
    etdf <- foreach(j=1:length(uids), .combine = rbind) %dopar% {
      
      xx <- filter(etd, id == uids[j]) %>%
        mutate(x = x + (resolution/2),
               y = y - (resolution/2),
               id = id+tilenum) %>%
        distinct(x,y,id, .keep_all = T) %>%
        st_as_sf(coords = c("x","y"),crs = proj_modis) %>%
        st_intersection(wb)%>%
        group_by(id) %>%
        st_buffer(dist = 1+(resolution/2), endCapStyle = "SQUARE") %>%
        summarize(start_date = first(date),
                  last_date = last(date),
                  country_num = get_mode(country_num),
                  n_countries = length(unique(country_num)),
                  continent_num = get_mode(cont_num))
      
      return(xx)
    
    }

    conts <- unique(etdf$continent_num)
    
    for(c in 1:length(conts)){
      dd <- filter(etdf, continent_num == conts[c])
      fn <- paste0("data/wb_extracts/", tile, "_", conts[c],".gpkg")
      st_write(etdf, fn, delete_dsn = TRUE)
      system(paste0("aws s3 sync", " ",
                    "data/wb_extracts/", fn, " ", "
                  s3://earthlab-natem/modis-burned-area/delineated_events/world/wb_extracts/", fn ))
    }
    
    
   
  }
}


######------ progress thus far

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
