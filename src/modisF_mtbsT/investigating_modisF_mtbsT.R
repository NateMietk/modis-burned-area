# subsetting the mtbs perimters to those which modis did not detect
# author: Adam Mahood

source("src/a_prep_environment.R")
library(units)

space <- 5
time <- 11
file <- paste0("long_cast_s", space, "t",time, ".csv")

# getting data ------------------------------------------------------------
system(paste0("aws s3 cp ",
              "s3://earthlab-natem/modis-burned-area/MCD64A1/C6/long_tables_casted/",
              file, " ",
              "data/tables/", file))

mtbs_shp <- file.path(mtbs_prefix, 'mtbs_perimeter_data_v2','dissolve_mtbs_perims_1984-2015_DD_20170501.shp')

if (!file.exists(mtbs_shp)) {
  loc <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/composite_data/burned_area_extent_shapefile/mtbs_perimeter_data.zip"
  dest <- paste0(mtbs_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = mtbs_prefix)
  unlink(dest)
  assert_that(file.exists(mtbs_shp))
}

# processing data -----------------------------------------------------
if(!exists("mtbs")){
  mtbs <- st_read(mtbs_shp) %>%
    st_cast(to = "MULTIPOLYGON") %>%
    st_cast(to = "POLYGON")
  mtbs$duped <- duplicated(mtbs$Fire_ID)
  
  mtbs$mtbs_cast_id <- ifelse(mtbs$duped == TRUE,
                        paste(as.character(mtbs$Fire_ID),as.character(row_number(mtbs$Fire_ID)), sep="_"),
                        as.character(mtbs$Fire_ID))
  
  mtbs$cast_area_ha <- st_area(mtbs[0])%>% set_units(value = hectare)
  mtbs$cast_area_ac <- st_area(mtbs[0])%>% set_units(value = acre)
}

long_table <- read.csv(paste0("data/tables/",file), stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  dplyr::filter(is.na(modis_id) == T)

mtbsT_modisF <- semi_join(mtbs, long_table, by = "mtbs_cast_id")

st_write(mtbsT_modisF, "mtbsT_modisF.gpkg")
system("aws s3 cp mtbsT_modisF.gpkg s3://earthlab-natem/modis-burned-area/mtbsT_modisF.gpkg")

bigs <- filter(mtbsT_modisF, as.numeric(cast_area_ha) > 5000) %>%
  arrange(desc(cast_area_ha)) %>%
  mutate(state = substr())

round(table(mtbs$FireType) / nrow(mtbs),3)

# Rx    RX   UNK    WF   WFU 
# 0.010 0.221 0.188 0.569 0.011 
# > round(table(mtbsT_modisF$FireType) / nrow(mtbsT_modisF),3)
# 
# Rx    RX   UNK    WF   WFU 
# 0.019 0.434 0.137 0.403 0.007 
# > summary(mtbsT_modisF$StartMonth)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   3.000   4.000   4.971   7.000  12.000 
# > summary(mtbsT_modisF[mtbsT_modisF$FireType == "WF",]$StartMonth)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   4.000   6.000   5.979   8.000  12.000 
# > hist(mtbsT_modisF[mtbsT_modisF$FireType == "WF",]$StartMonth)
# > hist(mtbsT_modisF$StartMonth)
# > hist(mtbsT_modisF[mtbsT_modisF$FireType == "RX",]$StartMonth)
# > hist(mtbs[mtbs$FireType == "WF",]$StartMonth)
# > hist(mtbsT_modisF[mtbsT_modisF$FireType == "WF",]$StartMonth)
# > hist(mtbs[mtbs$FireType == "RX",]$StartMonth)
# 
