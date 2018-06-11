source("src/a_prep_environment.R")
library(units)

space <- 1
time <- 1
years <- 2001:2015


dir.create("data/yearly_composites_15x15")
dir.create("data/long_tables/")
corz = detectCores()-1
registerDoParallel(corz)

# usa <- st_transform(usa, 4326)

foreach(TT = time) %:%
#for(TT in time){
  foreach(SS = space)%dopar% {
    
    # import files ------------------------------------------------------------------------------
    
    bt_fn <- paste0("big_table_s", SS,"t",TT,".csv")
    
    if(!file.exists(paste0("data/",bt_fn))){
      
    s3dir <- paste0("data/yearly_composites_15x15/s",SS,"t",TT)
    dir.create(s3dir)
    for(yy in years){
      s3file<- paste0("USA_BurnDate_",yy,"s",SS,"t",TT,".tif")
      if(!file.exists(paste0(s3dir, "/",s3file))){
      system(paste0("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_composites_15x15/",
                    s3file,
                    " ",
                    s3dir,"/",
                    s3file
      ))}
    }
    
    mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')
    
    if (!file.exists(mtbs_shp)) {
      loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
      dest <- paste0(mtbs_prefix, ".zip")
      download.file(loc, dest)
      unzip(dest, exdir = mtbs_prefix)
      unlink(dest)
      assert_that(file.exists(mtbs_shp))
    }
    
    # next thing
    
    
    #first big table -----------------------
    
    res_file <-paste0("mtbs_modis_ids_ba_cast_s",SS,"t",TT,".csv")
    if(!file.exists(paste0("data/",res_file))){
    
    results <- data.frame(Fire_ID = NA,
                          mtbs_cast_id = NA,
                          modis_id = NA, 
                          n = NA,
                          mtbs_acres = NA,
                          mtbs_hectares = NA,
                          mtbs_year = NA,
                          modis_ha = NA,
                          modis_acres = NA,
                          west_or_east = NA) #make a field with modis acres
    counter <- 1
    for(y in 1:length(years)){
      modis_y <- raster(paste0("data/yearly_composites_15x15/s",SS,"t",TT,"/",
                               "USA_BurnDate_",years[y],"s",SS,"t",TT,".tif"))

      if(!exists("modis_proj")){modis_proj <- crs(modis_y, asText=TRUE)} #set this
      
      if(!exists("mtbs")){
        mtbs <- st_read(mtbs_shp) %>%
          st_intersection(., st_union(st_transform(usa, st_crs(.)))) %>%
          st_transform(crs = modis_proj)}%>%
          st_cast(to = "POLYGON")
        mtbs$duped <- duplicated(mtbs$Fire_ID)
        
        mtbs$new_id <- ifelse(mtbs$duped == TRUE,
                              paste(as.character(mtbs$Fire_ID),as.character(row_number(mtbs$Fire_ID)), sep="_"),
                              as.character(mtbs$Fire_ID))
        
        mtbs$cast_area_ha <- st_area(mtbs[0])%>% set_units(value = hectare)
        mtbs$cast_area_ac <- st_area(mtbs[0])%>% set_units(value = acre)
      }
    
      mtbs_y <- mtbs[mtbs$Year == years[y],] 
      
      for(f in 1:nrow(mtbs_y)){
        fire <- mtbs_y[f,]
        cropped <- raster::crop(modis_y, as(fire, "Spatial"), snap = 'out')
        SpP_ras <- rasterize(fire, cropped, getCover=TRUE)
        SpP_ras[SpP_ras==0] <- NA
        SpP_ras[SpP_ras > 1] <- 1
        masked <- cropped * SpP_ras
        
        vc <- unique((masked[masked>0]))
        vc <- vc + as.numeric(paste0(years[y],"00000"))
        if(length(vc) == 0){vc<-NA}
        
        
        bpix_nobuff <- table(getValues(masked)) 
        barea_ha_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 21.4369
        barea_ac_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 52.9717335
        
        
        w_e <-as.character(ifelse(st_bbox(st_transform(fire,4326))[1] < -97, "w", "e"))
        
        results[counter, 1] <- as.character(fire$Fire_ID)
        results[counter, 2] <- as.character(fire$new_id)
        results[counter, 3] <- paste(as.character(vc), collapse = " ")
        results[counter, 4] <- length(vc[!is.na(vc)])
        results[counter, 5] <- fire$cast_area_ac
        results[counter, 6] <- fire$cast_area_ha
        results[counter, 7] <- fire$Year
        results[counter, 8] <- barea_ha_nobuff
        results[counter, 9] <- barea_ac_nobuff
        results[counter, 10] <- w_e
        print(c(counter, years[y]))
        counter <- counter + 1
      }
    
    
    write.csv(results, paste0("data/",res_file))
    system(paste0("aws s3 cp data/",res_file," s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables_casted/",res_file))
    }else{results <- read.csv(paste0("data/",res_file), stringsAsFactors = FALSE)}
     # breaking it down to just mtbsIDs and modis IDs ------------------
     long_mt_mo <- data.frame(Fire_ID=NA, mtbs_cast_id=NA, modis_id=NA)
     counter <- 1
     for(i in 1:nrow(results)){
       ss <- strsplit(results$modis_id[i], " ") %>% unlist()
       if(length(ss)>0){for(j in 1:length(ss)){
         long_mt_mo[counter, 1] <- results$Fire_ID[i]
         long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
         long_mt_mo[counter, 3] <- ss[j]
         counter <- counter + 1
       }}else{
         long_mt_mo[counter, 1] <- results$Fire_ID[i]
         long_mt_mo[counter, 2] <- results$mtbs_cast_id[i]
         long_mt_mo[counter, 3] <- NA
         counter <- counter + 1}
     }
     gc()
     
     
     longfile = paste0("long_cast_s",SS,"t",TT,".csv")
     write.csv(long_mt_mo, paste0("data/long_tables/",longfile))
     system(paste0("aws s3 cp data/long_tables/",
                   longfile,
                   " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/long_tables_casted/",
                   longfile))
     
     # calculate modis id numbers -----------------------------------
     e_th <- 202/21.4369 #thresholds in pixels
     w_th <- 404/21.4369
     
     m_ids <- data.frame(year = NA, n_ids = NA, n_over_th = NA)
     for(i in 1:length(years)){
       print(years[i])
       stem <- paste0("USA_BurnDate_",years[i],"s",SS,"t",TT)
       
       r <- raster(paste0("data/yearly_composites_15x15/s",SS,"t",TT,"/",
                                stem,".tif"))
       m_ids[i,1] <- years[i]
       m_ids[i,2] <- length(unique(freq(r)))
       print("freq")
       
       r[r<1] <- NA
       geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
       d <- rasterToPoints(r, spatial=TRUE) %>%
              spTransform(CRS(geo.prj))
       d <- data.frame(d@data, long=coordinates(d)[,1]) 
       names(d) <- c("a", "long")
       d <- as_tibble(d) %>%
         group_by(a) %>%
         summarize(pixel_count = n(),
                   long = mean(long))
       d$th <- ifelse(d$long < -97, w_th,e_th)
       
       m_ids[i,3] <- nrow(d[d$th < d$pixel_count, ])
       print("over_th")
     }
     dir.create("data/m_ids/")
     write.csv(m_ids, paste0("data/m_ids/m_ids_s",SS,"t",TT,".csv"))
     # big table time baby ----------------------------------
     
     big_table <- data.frame(modisT_mtbsT = NA,
                             modisF_mtbsT = NA,
                             modisT_mtbsF_all_modis = NA,
                             modisT_mtbsF_modis_over_th = NA,
                             st_combo = NA,
                             mtbs_w_multiple_modis = NA,
                             modis_w_multiple_mtbs = NA,
                             mean_n_modis_per_mtbs = NA,
                             median_n_modis_per_mtbs = NA,
                             max_n_modis_per_mtbs = NA,
                             which_max_modis_per_mtbs = NA,
                             mean_n_mtbs_per_modis = NA,
                             median_n_mtbs_per_modis = NA,
                             max_n_mtbs_per_modis = NA,
                             which_max_mtbs_per_modis = NA,
                             row_check = NA,
                             mtbsT_modisT_unique_modis_events = NA,
                             mtbsT_modisT_total_n_modis_events_with_repeats = NA,
                             space = NA,
                             time = NA,
                             mtbs_IDs_of_max_modis = NA)
     
     rc = table(results$n) %>% as_tibble()
     NN <- sum(rc$n[1:2])
     rc <- rc[3:nrow(rc),]
     for(ROW in 1:nrow(rc)){
       XX <- as.numeric(rc$Var1[ROW])
       NN <- NN + (XX * rc$n[ROW])
     }
     
     n_modis_per_mtbs <- table(long_mt_mo$Fire_ID) %>% 
       as_tibble() %>%
       filter(Var1 != "NA")
     
     n_mtbs_per_modis <- table(long_mt_mo$modis_id) %>% 
       as_tibble() %>%
       filter(Var1 != "NA")
     
     big_table[1, 1] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 3] <- sum(m_ids$n_ids, na.rm=T) - length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     big_table[1, 4] <- sum(m_ids$n_over_th, na.rm=T) - length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$mtbs_cast_id))
     
     big_table[1,5] <- paste0("s",SS,"t",TT)
     big_table[1,6] <- nrow(n_modis_per_mtbs[n_modis_per_mtbs$n > 1,])
     big_table[1,7] <- nrow(n_mtbs_per_modis[n_mtbs_per_modis$n > 1,])
     big_table[1,8] <- mean(n_modis_per_mtbs$n)
     big_table[1,9] <- median(n_modis_per_mtbs$n)
     
     max1 <- max(n_modis_per_mtbs$n)
     
     big_table[1,10] <- max1
     
     big_table[1,11] <- paste(results[results$n == max1,]$mtbs_cast_id, collapse = " ")
     big_table[1,12] <- mean(n_mtbs_per_modis$n)
     big_table[1,13] <- median(n_mtbs_per_modis$n)
     
     max2 <- max(n_mtbs_per_modis$n)
     
     big_table[1,14] <- max2
     
     which2 <- as.numeric(n_mtbs_per_modis[n_mtbs_per_modis$n == max2,]$Var1)
     
     big_table[1,15] <- paste(as.character(which2), collapse = " ")
     big_table[1,16] <- NN==nrow(long_mt_mo)
     big_table[1,17] <- length(unique(long_mt_mo$modis_id))
     big_table[1,18] <- sum(results$n)
     big_table[1,19] <- SS
     big_table[1,20] <- TT
     big_table[1,21] <- paste(as.character(dplyr::filter(long_mt_mo, modis_id == first(which2))$mtbs_cast_id),collapse = " ")
     
     
     write.csv(big_table, paste0("data/",bt_fn))
     system(paste0("aws s3 cp data/",bt_fn, 
                   " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables_casted/",bt_fn))
     print(Sys.time()-t0)
     system(paste0("rm -r ",s3dir))
    }
   }
# }

# stiching together the final table ----------------------------------
dir.create("data/final_tables")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables_casted data/final_tables")

tables <- list.files("data/final_tables")
table_l <- list()
for(i in 1:length(tables)){
  table_l[[i]] <- read.csv(paste0("data/final_tables/",tables[i]), stringsAsFactors = FALSE)
}

final_table <- do.call("rbind", table_l) %>% as_tibble()
final_table$modisF_mtbsT<-4223 #whatever... obtained from fixing_confusing_matrix.R
final_table <- final_table[,-c(1,7:12)]
write.csv(final_table, "data/confusion_matrices.csv")
system("aws s3 cp data/confusion_matrices.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/confusion_matrix/confusion_matrices.csv")



# thanks https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
