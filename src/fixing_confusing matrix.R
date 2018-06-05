source("src/a_prep_environment.R")


space <- 1:15
time <- 1:15
kounter = 1

dir.create("data/tables")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/result_tables data/tables")

big_table <- data.frame(st_combo = NA, modisF_mtbsT = NA,r2_u10000 = NA,
                        r2_o10000 = NA,
                        r2_all = NA,
                        coef_u10000 = NA,
                        coef_o10000 = NA,
                        coef_all = NA)

for(SS in space){
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
    
    big_table[kounter,1] <- paste0("s",SS,"t",TT)
    big_table[kounter,2] <- length(unique(long_mt_mo[is.na(long_mt_mo$modis_id),]$Fire_ID))
    
    mod1 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres <10000,])
    mod2 <- lm(modis_acres ~ -1+mtbs_acres, data = results[results$mtbs_acres >10000,])
    mod3 <- lm(modis_acres ~ -1+mtbs_acres, data = results)
    
    big_table[kounter, 3] <- summary(mod1)$r.squared
    big_table[kounter, 4] <- summary(mod2)$r.squared
    big_table[kounter, 5] <- summary(mod3)$r.squared
    big_table[kounter, 6] <- as.numeric(mod1$coefficients)
    big_table[kounter, 7] <- as.numeric(mod2$coefficients)
    big_table[kounter, 8] <- as.numeric(mod3$coefficients)
    kounter = kounter +1
    print(c(SS,TT))
    print(Sys.time())
    print(Sys.time() - t0)
  }
}

for(SS in space){
  for(TT in time){
    results <- read.csv(paste0("data/tables/mtbs_modis_ids_ba_s",SS,"t",TT,".csv"),
                        stringsAsFactors = F) %>% as_tibble()
    print(c(SS,TT))
    print(summary(results$n))
}}

