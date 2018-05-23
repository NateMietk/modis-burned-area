source("src/a_prep_environment.R")
install.packages("tabularaster")
library(tabularaster)

space <- 1:15
time <- 1:15

dir.create("data/yearly_composites_15x15")
corz = detectCores()/4
registerDoParallel(corz)

foreach(SS = space) %dopar% {
  for(TT in time){
    
    # import files ------------------------------------------------------------------------------
    
    t0 <- Sys.time()
    kounter <- 1
    years <- 2001:2015
    
    for(yy in years){
      s3file<- paste0("USA_BurnDate_",yy,"s",SS,"t",TT,".tif")
      system(paste0("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_composites_15x15/",
                    s3file,
                    " data/yearly_composites_15x15/",
                    s3file
      ))
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
      modis_y <- raster(paste0("data/yearly_composites_15x15/USA_BurnDate_",years[y],"s",SS,"t",TT,".tif"))
      modis_y <- modis_y + as.numeric(paste0(years[y],"00000"))
      
      if(!exists("modis_proj")){modis_proj <- crs(modis_y, asText=TRUE)} #set this
      
      if(!exists("mtbs")){
        mtbs <- st_read(mtbs_shp) %>%
          st_intersection(., st_union(st_transform(usa, st_crs(.)))) %>%
          st_transform(crs = modis_proj)}
      
      if(!exists("m97")){m97 <- st_sf(a=1,geom = st_sfc(st_point(c(-97,45))))%>%
        st_set_crs(4326) %>%
        st_transform(crs = modis_proj) %>%
        st_bbox() %>%
        as.numeric()
      m97 <- m97[1]}
      
      mtbs_y <- mtbs[mtbs$Year == years[y],] 
      
      
      
      for(f in 1:nrow(mtbs_y)){
        fire <- mtbs_y[f,]
        cropped <- raster::crop(modis_y, as(fire, "Spatial"), snap = 'out')
        # thanks https://gis.stackexchange.com/questions/255025/r-raster-masking-a-raster-by-polygon-also-remove-cells-partially-covered
        SpP_ras <- rasterize(fire, cropped, getCover=TRUE)
        SpP_ras[SpP_ras==0] <- NA
        SpP_ras[SpP_ras > 1] <- 1
        masked <- cropped * SpP_ras
        
        vc <- unique(getValues(masked))
        vc <- vc[vc>0]
        vc <- ifelse(vc == as.numeric(paste0(years[y],"00000")), NA, vc)
        if(length(vc) == 0){vc<-NA}
        
        bpix_nobuff <- table(getValues(cropped)) 
        barea_ha_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 21.4369
        barea_ac_nobuff <- sum(bpix_nobuff[2:length(bpix_nobuff)]) * 52.9717335
        
        w_e <-ifelse(st_bbox(fire)[1] < m97, "w", "e")
        
        results[counter, 1] <- as.character(fire$Fire_ID)
        results[counter, 2] <- paste(as.character(vc[vc>0 & !is.na(vc)]), collapse = " ")
        results[counter, 3] <- length(vc[vc>0 & !is.na(vc)])
        results[counter, 4] <- fire$Acres
        results[counter, 5] <- fire$Year
        results[counter, 6] <- barea_ha_nobuff
        results[counter, 7] <- barea_ac_nobuff
        results[counter, 8] <- w_e
        
        
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
     
     # calculate modis id numbers -----------------------------------
     tabs <-  list()
     vals <- list()
     m_ids <- data.frame(year = NA, n_ids = NA)
     for(i in 1:length(years)){
       modis_y <- raster(paste0("data/yearly_composites_15x15/USA_BurnDate_",years[i],"s",SS,"t",TT,".tif"))
       modis_y <- modis_y + as.numeric(paste0(years[i],"00000"))
       vals[[i]] <- as_tibble(modis_y,xy=T) %>%
         na.omit()
       vals[[i]] <- vals[[i]][vals[[i]]$cellvalue>0,]
       vals[[i]]$cellvalue <- as.character(ifelse(vals[[i]]$cellvalue == as.numeric(paste0(years[y],"00000")), 
                                                  NA, vals[[i]]$cellvalue))
       vals[[i]] <- na.omit(vals[[i]])
       vals[[i]]$w <-ifelse(vals[[i]]$x < m97, 'w', 'e')
       m_ids[i,1] <- years[i]
       m_ids[i,2] <- length(unique(vals[[i]]$cellvalue))
       we <- vals[[i]] %>% 
         dplyr::select(w_e, cellvalue) %>% 
         group_by(cellvalue) %>%
         summarise(w_e = first(w))
       tabs[[i]] <- as_tibble(table(vals[[i]]$cellvalue))%>% 
         rename(cellvalue = Var1) %>%
         left_join(y=we)  %>% 
         rename(modis_id = cellvalue)
     }
     
     all_modis_fires <- do.call("rbind", tabs) %>%
       mutate(ba_ha = n * 21.4369,
              ba_ac = n * 52.9717335)
     
     over_th_w <- all_modis_fires %>%
       filter(w_e == "w") %>%
       filter(ba_ha > 404)
     
     over_th_e <- all_modis_fires %>%
       filter(w_e == "2") %>%
       filter(ba_ha > 202)
     
     all_over_th <- rbind(over_th_e,over_th_w) 
     
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
                             coef_all = NA)
     
     big_table[kounter, 1] <- length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[kounter, 2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[kounter, 3] <- sum(m_ids$n_ids) - length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$Fire_ID))
     big_table[kounter, 4] <- nrow(all_over_th) - length(unique(long_mt_mo[!is.na(long_mt_mo$modis_id),]$Fire_ID))
     
     t <- table(results$n)
     big_table[kounter, 5] <- sum(t[3:length(t)])
     
     mod1 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres <10000,])
     mod2 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres >10000,])
     mod3 <- lm(modis_acres ~ -1+mtbs_acres, data = results)
     
     big_table[kounter, 6] <- summary(mod1)$r.squared
     big_table[kounter, 7] <- summary(mod2)$r.squared
     big_table[kounter, 8] <- summary(mod3)$r.squared
     big_table[kounter, 9] <- as.numeric(mod1$coefficients)
     big_table[kounter, 10] <- as.numeric(mod2$coefficients)
     big_table[kounter, 11] <- as.numeric(mod3$coefficients)
     
     bt_fn <- paste0("big_table_s", SS,"t",TT,".csv")
     write.csv(big_table, paste0("data/",bt_fn))
     system(paste0("aws s3 cp data/",bt_fn, 
                   " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/final_tables/",bt_fn))
     print(Sys.time()-t0)
     kounter <- kounter + 1
     system("rm -r data/yearly_composites_15x15/*")
  }
}
