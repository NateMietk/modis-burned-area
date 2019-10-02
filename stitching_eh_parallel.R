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
install.packages("mapedit") # this is supposed to have a rbind for sf objects
libs <- c("tidyverse", "raster", "mapedit", "foreach", "doParallel","sf","funique", "units")
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
# first create points and overlay with continent ===============================
# load in world boundaries and select relevant columns =======================
dir.create("data/wb_extracts")
wb <- st_read("data/wb") %>%
  dplyr::select(name = NAME,
                continent = CONTINENT) %>%
  mutate(cont_num = as.numeric(continent),
         country_num = as.numeric(name)) %>%
  dplyr::select(cont_num, country_num) %>%
  st_transform(crs=st_crs(proj_modis))

# convert csv to polygons and overlay with continents ========================
edge_tile_files <- list.files("data/edge_tiles", pattern = "csv", full.names = TRUE)[57:169]

system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/wb_extracts data/wb_extracts")
tiles_done <- list.files("data/wb_extracts/") %>% str_extract("h\\d{2}v\\d{2}")

for(i in 1:length(edge_tile_files)){
  tile <- edge_tile_files[i] %>% str_extract("h\\d{2}v\\d{2}")
  
  if(!tile %in% tiles_done){
    system(paste("echo", tile))
    
    
    tilenum <- paste0("1",
                      str_sub(tile, 2,3), 
                      str_sub(tile,5,6),
                      "0000000") %>% 
      as.numeric
    
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
        summarize(start_date = min(date),
                  last_date = max(date),
                  country_num = get_mode(country_num),
                  n_countries = length(unique(country_num)),
                  continent_num = get_mode(cont_num))
      
      return(xx)
    
    }

    conts <- unique(etdf$continent_num)
    
    for(c in 1:length(conts)){
      dd <- filter(etdf, continent_num == conts[c])
      fn <- paste0(tile, "_", conts[c],".gpkg")
      st_write(dd, file.path("data/wb_extracts",fn), delete_dsn = TRUE)
      system(paste0("aws s3 cp", " ",
                    "data/wb_extracts/", fn, " ",
                    "s3://earthlab-natem/modis-burned-area/delineated_events/world/wb_extracts/", fn ))
    }
    
    
   
  }
}


# then create buffer polygons by continent ==================================
# creating the buff # 1 min wh
dir.create("data/eh_buffers")
# system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_buffers data/eh_buffers")
tile_polys<-list.files("data/wb_extracts", full.names = TRUE, pattern="gpkg")

for(i in 1:length(tile_polys)) {
  ddb <- st_read(tile_polys[i], quiet = TRUE)
  
  corz<- detectCores()-1
  registerDoParallel(corz)
  xx <- foreach(j=1:nrow(ddb), .combine = rbind) %dopar%{
    return(st_buffer(ddb[j,], dist = (ss/2)+1))
   
  }
  fn <- str_c(tile_polys[i]%>% str_sub(18,25), "_", "buff.gpkg")
  st_write(xx, paste0(
      "data/eh_buffers/", fn
    ), quiet = TRUE)
  system(paste0("aws s3 cp ",
                "data/eh_buffers/", fn, " ",
                "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_buffers/", fn))
}


# rbind continents for buffers and regular polygons ========================

# system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_continent_files data/continent_files")

buff_files <- list.files("data/eh_buffers", pattern = "gpkg", full.names = TRUE)

dir.create("data/continent_files")
conts <- wb$cont_num %>% unique()
buff_list<-list()
for(i in 1:length(conts)){
  files <- buff_files[str_detect(buff_files, str_c("_", i, "_"))]
  if(length(files>0)){
  for(j in 1:length(files)){
    buff_list[[j]] <- st_read(files[j])
  }
  full_thing<-do.call("rbind", buff_list)
  fn <- str_c("buffs_cont_", i, ".gpkg")
  st_write(full_thing, file.path(str_c("data/continent_files/", fn)))
  system(str_c("aws s3 cp ",
               "data/continent_files/", fn, " ",
               "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_continent_files/",fn))
  }
}

poly_files <- list.files("data/wb_extracts", pattern = "gpkg", full.names = TRUE)

poly_list<-list()
for(i in 1:length(conts)){
  files <- poly_files[str_detect(poly_files, str_c("_", i, "."))]
  if(length(files>0)){
    for(j in 1:length(files)){
      poly_list[[j]] <- st_read(files[j])
    }
    full_thing<-do.call("rbind", poly_list)
    fn <- str_c("polys_cont_", i, ".gpkg")
    st_write(full_thing, file.path(str_c("data/continent_files/", fn)))
    system(str_c("aws s3 cp ",
                 "data/continent_files/", fn, " ",
                 "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_continent_files/",fn))
  }
}


######------ progress thus far

# function to Paralise any simple features analysis. ================================
st_parallel <- function(sf_df, sf_func, n_cores, ...){
  
  # Create a vector to split the data set up by.
  split_vector <- rep(1:n_cores, each = nrow(sf_df) / n_cores, length.out = nrow(sf_df))
  
  # Perform GIS analysis
  split_results <- split(sf_df, split_vector) %>%
    parallel::mclapply(function(x) sf_func(x, ...), mc.cores = n_cores)
  
  
  # Define the output_class. If length is greater than two, then grab the second variable.
  output_class <- class(split_results[[1]])
  if (length(output_class) == 2){
    output_class <- output_class[2]
  }
  
  # Combine results back together. Method of combining depends on the output from the function.
  if (output_class == "matrix"){
    result <- do.call("rbind", split_results)
    names(result) <- NULL
  } else if (output_class == "sfc") {
    result <- do.call("c", split_results)
    result <- sf_func(result) # do.call combines the list but there are still n_cores of the geometry which had been split up. Running st_union or st_collect gathers them up into one, as is the expected output of these two functions. 
  } else if (output_class %in% c('list', 'sgbp') ){
    result <- do.call("c", split_results)
    names(result) <- NULL
  } else if (output_class == "data.frame" ){
    result <- do.call("rbind", split_results)
  } else {
    stop("Unknown class. st_parallel only accepts the following outputs at present: sfc, list, sf, matrix, sgbp.")
  }
  
  # Return result
  return(result)
}

# join the edges together =======================================================

system(str_c("aws s3 sync ",
             "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_continent_files ",
             "data/continent_files "
             ))
conts <- c(3,4,5,6,7,1)

t0 <- Sys.time()

for(c in conts){
  fn <- paste0("cont_",c,"_edges_stitched.gpkg")
  if(!file.exists(str_c("data/",fn))){
    oo <- st_read(paste0("data/continent_files/buffs_cont_", c, ".gpkg")) %>%
      dplyr::select(geom, start_date, last_date,id) %>%
      mutate(tile = str_sub(id, 2,5))
    
    dd <- st_read(paste0("data/continent_files/polys_cont_", c, ".gpkg")) %>%
      dplyr::select(-country_num, -n_countries, -continent_num)
    
    # parallel won't work for st_overlap!!!
    # parallelize by continent first and save the matrix as an rds
    # x <- st_parallel(ff, st_overlaps, detectCores()-1)
    t0 <- Sys.time()
    #x <- st_overlaps(oo, sparse = TRUE) 
    oo <- oo[1:1000,]
    registerDoParallel(detectCores()-1)
    x <- foreach(i = 1:nrow(oo))%dopar%{
      current_tile <- oo[i,]$tile
      zz<- st_overlaps(oo[i,], oo)[[1]]#;zz
      zzz<-vector(length=length(zz), mode = "numeric")
      zzz<-NA
      if(length(zz)>0){
      for(j in 1:length(zz)){
        # add in tiles being different? maybe the above is good
          if(oo[i,]$start_date %in%
             oo[zz[j],]$start_date:oo[zz[j],]$last_date |
             oo[i,]$last_date %in%
             oo[zz[j],]$start_date:oo[zz[j],]$last_date){
            if(oo[i,]$tile != oo[zz[j],]$tile){zzz[j] <- zz[j]}
          }
        
        }
      }
      return(funique(na.omit(zzz)))
      # zzz<- na.omit(zzz)
      # dd[zzz,]$id <- min(dd[zzz,]$id)
    }
    
    #saveRDS(x, paste0("cont_",c,"overlaps.rds"))
    print(Sys.time() - t0)
    
    xx <- vector(mode = "logical", length = length(x))
    for(i in 1:length(x)){
      xx[i] <- ifelse(length(na.omit(x[[i]])) > 0, TRUE, FALSE)
    }
    xxx<- xx
    
    
    res <- list()
    counter <- 1 # counting changes
    
    orig_dd_ids <- dd$id
    # registerDoParallel(detectCores()-1)
    #res <- foreach(i = 1:nrow(dd), .combine = rbind)%dopar%{
    #for(i in 1:nrow(dd)){
    for(i in 1:length(x)){
        
      if(xx[i] == TRUE){
        print(paste("echo", i,"doin' it", round(i/nrow(dd) * 100,3), "%"))
        
        # figureing out which rows to select
        rows <- x[[i]]
        rows1 <- c(i,rows, x[rows]) %>% unlist() %>% funique()
        while(length(rows1) != length(rows)){
          rows <- c(i, rows1, x[rows1]) %>% unlist() %>% funique()
          rows1 <- c(i,rows, x[rows]) %>% unlist() %>% funique()
        }
        
        # making sure we dont repeat things
        xx[rows1] <- FALSE
        #xx[rows] <- FALSE
        # selecting the rows
        dd_subset <- dd[rows1,] %>%
          mutate(flag = FALSE)
        orig_ids <- dd_subset$id
        
        if(length(unique(str_sub(orig_ids,1,5)))>1){
          for(j in 1:nrow(dd_subset)){
            if(orig_ids[j] == dd_subset$id[j]){
              if(!dd_subset$flag[j])
              # determining the temporal overlap in a rough but efficient way
              start_range <- dd_subset$start_date[j] - ttime
              end_range <- dd_subset$last_date[j] + ttime
              range <- start_range:end_range
              ww <- which(as.numeric(dd_subset[dd_subset$id != 
                                                 dd_subset$id[j],]$start_date)
                          %in% range)
              ww <- c(ww, 
                      which(as.numeric(dd_subset[dd_subset$id != 
                                                   dd_subset$id[j],]$last_date) 
                            %in% range)) %>% funique
              
              # this is doing the same thing over and over and over but whatever who cares it doesn't add time
              # .... or we should fix it 
              if(length(ww) > 1){
                new_id <- dd_subset[ww,]$id %>% min()
                dd[rows1[ww],]$id <- new_id
                print(paste("something happened at", i))
                dd_subset[ww,]$flag <- TRUE
                #counter = counter +1
              }
              # if(length(ww) > 1){
              #   dd_subset[ww,]$id <- dd_subset[ww,]$id %>% min()
              #   dd_subset[ww,]$flag <- TRUE
              #   print(paste("something happened at", i))
              # }
            }
          }
        }
    
        #return(dd_subset)
        #res[[counter]] <- dd_subset %>% dplyr::select(-flag)
        #counter <- counter + 1
      }else{print(paste("echo",i,"skipin' it", round(i/nrow(dd) * 100, 3), "%"))
        #res[[counter]] <- dd[i,]
        #counter <- counter +1
        }
    }
    # done <- do.call('rbind', res)
    # devtools::install_github("tidyverse/multidplyr")
    #cluster <- new_cluster(detectCores()-1)
    
    print(Sys.time()-t0)
    
    
    # registerDoParallel(detectCores()-1)
    # gg<- foreach(i = 1:length(res))%dopar%{
    #   return(res[[i]] %>%
    #     group_by(id) %>%
    #     summarise(start_date = min(start_date),
    #               last_date = max(last_date)) %>%
    #     mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) %>%
    #       st_as_sf() %>%
    #       st_cast("MULTIPOLYGON")
    #   )
    # }
    
    gg <- dd %>%
      group_by(id) %>%
      mutate(n = n()) %>%
      ungroup()

    # hh <- dd[which(xxx == FALSE),] %>%
    #   mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) 
    
    ii<- gg %>%
      filter(n ==1) %>%
      dplyr::select(-n) %>%
      mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) 
    
   
      jj <- gg %>%
        filter(n > 1) %>%
        dplyr::select(-n) %>%
        group_by(id) %>%
        summarise(start_date = min(start_date),
                  last_date = max(last_date)) %>%
        mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) 
      
      kk<- do.call("rbind", list(jj,ii,hh))
    
    
    st_write(kk,paste0("data/", fn), delete_dsn=TRUE) 
    system(str_c("aws s3 cp ",
                 "data/", fn, " ",
                 "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_stitched_edges/", fn))
    
    # gg <- st_read(paste0("data/continent_files/polys_cont_", c, ".gpkg")) %>%
    #   tibble::rownames_to_column("row")%>%
    #   dplyr::select(-id, -last_date,-start_date, -country_num, -continent_num, -n_countries) %>%
    #   left_join(dd, by = "row") %>%
    #   dplyr::select(-row) %>%
    #   group_by(id) %>%
    #   mutate(n = n()) %>%
    #   ungroup()
    # 
    # ff <- filter(gg, n==1) %>%
    #   dplyr::select(-n) %>%
    #   mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) 
    # 
    # ee <- filter(gg, n>1)%>%
    #   group_by(id) %>%
    #   summarise(start_date = min(start_date),
    #             last_date = max(last_date)) %>%
    #   mutate(area_burned_ha = drop_units(st_area(.)*0.0001)) 
    # 
    # gg<- rbind(ff, ee)
    # st_write(gg,paste0("data/", fn)) 
    # system(str_c("aws s3 cp ",
    #              "data/", fn, " ",
    #              "s3://earthlab-natem/modis-burned-area/delineated_events/world/eh_stitched_edges/", fn))
  }
}
source("src/r/rbind_eh.R")
print(Sys.time()-t0)


