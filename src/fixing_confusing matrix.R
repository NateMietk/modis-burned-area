source("src/a_prep_environment.R")


space <- 1:15
time <- 1:15
kounter = 1

corz = detectCores()-1
registerDoParallel(corz)

dir.create("data/tables")
dir.create("data/long_tables")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables data/tables")

big_table <- tibble(st_combo = NA,
                        mtbs_w_multiple_modis = NA,
                        modis_w_multiple_mtbs = NA,
                        mean_n_modis_per_mtbs = NA,
                        median_n_modis_per_mtbs = NA,
                        max_n_modis_per_mtbs = NA,
                        which_max_modis_per_mtbs = NA,
                        mean_n_mtbs_per_modis = NA,
                        median_n_mtbs_per_modis = NA,
                        max_n_mtbs_per_modis = NA,
                        which_max_mtbs_per_modis = NA)

foreach(SS = space)%dopar%{
  for(TT in time){
    t0 <- Sys.time()
    results <- read.csv(paste0("data/tables/mtbs_modis_ids_ba_s",SS,"t",TT,".csv"),
                        stringsAsFactors = F) %>% as_tibble()
    
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
    
    longfile = paste0("long_s",SS,"t",TT,".csv")
    write.csv(long_mt_mo, paste0("data/long_tables/",longfile))
    system(paste0("aws s3 cp data/long_tables/",
                  longfile,
                  " s3://earthlab-natem/modis-burned-area/MCD64A1/C6/long_tables/",
                  longfile))
    
    n_modis_per_mtbs <- table(long_mt_mo$Fire_ID) %>% 
      as_tibble() %>%
      filter(Var1 != "NA")
    
    n_mtbs_per_modis <- table(long_mt_mo$modis_id) %>% 
      as_tibble() %>%
      filter(Var1 != "NA")
    
    big_table[kounter,1] <- paste0("s",SS,"t",TT)
    big_table[kounter,2] <- nrow(n_modis_per_mtbs[n_modis_per_mtbs$n > 1,])
    big_table[kounter,3] <- nrow(n_mtbs_per_modis[n_mtbs_per_modis$n > 1,])
    big_table[kounter,4] <- mean(n_modis_per_mtbs$n)
    big_table[kounter,5] <- median(n_modis_per_mtbs$n)
    big_table[kounter,6] <- max(n_modis_per_mtbs$n)
    big_table[kounter,7] <- n_modis_per_mtbs[max(n_modis_per_mtbs$n),]$Var1
    big_table[kounter,8] <- mean(n_mtbs_per_modis$n)
    big_table[kounter,9] <- median(n_mtbs_per_modis$n)
    big_table[kounter,10] <- max(n_mtbs_per_modis$n)
    big_table[kounter,11] <- n_mtbs_per_modis[max(n_modis_per_mtbs$n),]$Var1
    

    kounter = kounter +1
    print(c(SS,TT))
    print(Sys.time())
    print(Sys.time() - t0)
    
  }
}

write.csv(big_table, "data/repeats.csv")
system("aws s3 cp data/repeats.csv s3://earthlab-natem/modis-burned-area/MCD64A1/C6/confusion_matrix/repeats.csv")
