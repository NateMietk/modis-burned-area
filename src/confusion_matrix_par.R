source("src/a_prep_environment.R")
# install.packages("tabularaster")
# library(tabularaster)
# install.packages("velox")
# library(velox)

space <- 1:15
time <- 1:15
years <- 2001:2015


dir.create("data/yearly_composites_15x15")
corz = detectCores()-1
registerDoParallel(corz)

# usa <- st_transform(usa, 4326)

foreach(TT = time) %dopar% {
#for(TT in time){
  for(SS in space){
    
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
    
    results <- data.frame(Fire_ID = NA, 
                          modis_id = NA, 
                          n = NA,
                          mtbs_acres = NA,
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
          st_transform(crs = modis_proj)}

      mtbs_y <- mtbs[mtbs$Year == years[y],] 
      
      for(f in 1:nrow(mtbs_y)){
        fire <- mtbs_y[f,]
        cropped <- raster::crop(modis_y, as(fire, "Spatial"), snap = 'out')
        # thanks https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
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
        results[counter, 2] <- paste(as.character(vc), collapse = " ")
        results[counter, 3] <- length(vc[!is.na(vc)])
        results[counter, 4] <- fire$Acres
        results[counter, 5] <- fire$Year
        results[counter, 6] <- barea_ha_nobuff
        results[counter, 7] <- barea_ac_nobuff
        results[counter, 8] <- w_e
        print(c(counter, years[y]))
        counter <- counter + 1
      }
    }
    
    res_file <-paste0("mtbs_modis_ids_ba_s",SS,"t",TT,".csv")
    write.csv(results, paste0("data/",res_file ))
    system(paste0("aws s3 cp data/",res_file," s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables/",res_file))
   
     # breaking it down to just mtbsIDs and modis IDs ------------------
     long_mt_mo <- data.frame(Fire_ID=NA, modis_id=NA)
     counter <- 1
     for(i in 1:nrow(results)){
       ss <- strsplit(results$modis_id[i], " ") %>% unlist()
       if(length(ss)>0){for(j in 1:length(ss)){
         long_mt_mo[counter, 1] <- results$Fire_ID[i]
         long_mt_mo[counter, 2] <- ss[j]
         counter <- counter + 1
       }}else{
         long_mt_mo[counter, 1] <- results$Fire_ID[i]
         long_mt_mo[counter, 2] <- NA
         counter <- counter + 1}
     }
     gc()
     
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
     
     # big table time baby ----------------------------------
     
     big_table <- data.frame(modisT_mtbsT = NA,
                             modisF_mtbsT = NA,
                             modisT_mtbsF_all_modis = NA,
                             modisT_mtbsF_modis_over_th = NA,
                             mtbs_w_multiple_modis = NA,
                             r2_u10000 = NA,
                             r2_o10000 = NA,
                             r2_all = NA,
                             coef_u10000 = NA,
                             coef_o10000 = NA,
                             coef_all = NA,
                             st_combo = NA)
     
     big_table[1, 1] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[1, 2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[1, 3] <- sum(m_ids$n_ids, na.rm=T) - length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[1, 4] <- sum(m_ids$n_over_th, na.rm=T) - length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$Fire_ID))
     
     t <- table(results$n)
     big_table[1, 5] <- sum(t[3:length(t)])
     
     mod1 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres <10000,])
     mod2 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres >10000,])
     mod3 <- lm(modis_acres ~ -1+mtbs_acres, data = results)
     
     big_table[1, 6] <- summary(mod1)$r.squared
     big_table[1, 7] <- summary(mod2)$r.squared
     big_table[1, 8] <- summary(mod3)$r.squared
     big_table[1, 9] <- as.numeric(mod1$coefficients)
     big_table[1, 10] <- as.numeric(mod2$coefficients)
     big_table[1, 11] <- as.numeric(mod3$coefficients)
     big_table[1,12] <- paste0("s",SS,"t",TT)
     
     write.csv(big_table, paste0("data/",bt_fn))
     system(paste0("aws s3 cp data/",bt_fn, 
                   " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables/",bt_fn))
     print(Sys.time()-t0)
     system(paste0("rm -r ",s3dir))
    }
  }
}

# stiching together the final table ----------------------------------
dir.create("data/tables")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables data/tables")

tables <- list.files("data/tables")
table_l <- list()
for(i in 1:length(tables)){
  table_l[[i]] <- read.csv(paste0("data/tables/",tables[i]))
}

final_table <- do.call("rbind", table_l) %>% as_tibble()
