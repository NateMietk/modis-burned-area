# looking at fires mtbs captured and modis mcd64a1 missed
# author adam mahood

#checking to see percent of agriculture
libs("raster", "sf")
x <- "/home/a/data/landfire/US_140EVT_20180618.zip"
classes <- read.csv("/home/a/data/landfire/EVT_09152016.csv", stringsAsFactors = F)

getmode <- function(v) {
  uniqv <- na.omit(unique(v))
  uniqv[base::which.max(tabulate(match(v, uniqv)))]
}

# stolen from:
# 
# zip_name <- unlist(strsplit(x,"/"))
# zip_name <- zip_name[length(zip_name)]
# zip_folders <- unlist(strsplit(zip_name,split="_"))
# zip_folders <- gsub(paste(zip_folders[2:3],collapse="_"),pattern=".zip",replacement="")
# 
# system(paste("unzip -o ",x," -d /tmp",sep=""))

file <- file.path("/tmp","US_140EVT_20180618", "Grid", "us_140evt")#, "w001000.adf")
file.exists(file)
r <- raster(file)

mtbsT_modisF <- st_read("/home/a/Downloads/mtbsT_modisF.gpkg")

mtbsT_modisF$mode_evt <- raster::extract(r, mtbsT_modisF, fun= function(x,...) getmode(x))

st_write(mtbsT_modisF, "/home/a/Downloads/mtbsT_modisF_wEVT.gpkg")

mtbsT_modisF$VALUE <- as.numeric(mtbsT_modisF$mode_evt)

mtbsT_modisF1 <- dplyr::select(mtbsT_modisF, -mode_evt)%>%
  left_join(classes, by = "VALUE")

fire_veg <- as.data.frame(table(mtbsT_modisF1$CLASSNAME)) %>% as_tibble() %>%
  arrange(desc(Freq))

fire_EVT_LF <- as.data.frame(table(mtbsT_modisF1$EVT_LF)) %>% as_tibble() %>%
  arrange(desc(Freq))

fire_EVT_PHYS <- as.data.frame(table(mtbsT_modisF1$EVT_PHYS)) %>% as_tibble() %>%
  arrange(desc(Freq))

fire_EVT_CLASS <- as.data.frame(table(mtbsT_modisF1$EVT_CLASS)) %>% as_tibble() %>%
  arrange(desc(Freq))

fire_SAF_SRM <- as.data.frame(table(mtbsT_modisF1$SAF_SRM)) %>% as_tibble() %>%
  arrange(desc(Freq))
 