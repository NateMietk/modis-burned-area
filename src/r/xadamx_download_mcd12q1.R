# Script for downloading modis land cover type data -- MCD12Q1.006
# website: https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/
library(tidyverse)
library(raster)
library(gdalUtils)
library(sf)
dir.create("scrap/mcd12q1")

get_tiles <- function(aoi_mask){
  # aoi_mask = The shapefile mask where the tiles numbers are to be expected by.
  # This shapefile mask object is expected to be an sf object
  
  #Download the MODIS tile grid -------------------------
  dir.create("tmp", showWarnings = FALSE)
  dir_path <- file.path("tmp")
  loc <- "https://s3-us-west-2.amazonaws.com/modis-grids/modis_grid.zip"
  dest <- paste0(dir_path, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = dir_path)
  unlink(dest)
  
  modis_grid <- sf::st_read( file.path(dir_path, "modis_sinusoidal_grid_world.shp"), quiet= TRUE) %>%
    dplyr::mutate(h = if_else(nchar(as.character(h)) == 1, paste0("h0", as.character(h)), paste0("h", as.character(h))),
                  v = if_else(nchar(as.character(v)) == 1, paste0("v0", as.character(v)), paste0("v", as.character(v))),
                  hv = paste0(h, v, sep = " "))
  unlink(dir_path, recursive = TRUE)
  
  tiles <- aoi_mask %>%
    sf::st_transform(st_crs(modis_grid)) %>%
    st_intersection(modis_grid) %>%
    as.data.frame() %>%
    dplyr::select(hv) %>%
    distinct(., hv)
  return(as.vector(tiles$hv) %>%
           stringr::str_trim(., side = "both"))
}

download.file("https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
              "data/usa.zip")
unzip("data/usa.zip", exdir = "data")
# Import and prep the USA shapefile
usa <- st_read(file.path("data", "cb_2016_us_state_20m.shp"),
               quiet= TRUE) %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR"))) %>%
  dplyr::select(STUSPS)

# Vector of MODIS tiles to download
tiles <- get_tiles(usa) %>% sort
files<- list.files("/home/a/data/MCD12Q1/", full.names = T, pattern=".hdf") %>% sort()

for(i in 1:length(files)){
  sds <- gdalUtils::get_subdatasets(files[i])
  gdalUtils::gdal_translate(sds[1], dst_dataset = paste0("data/LC_", tiles[i],".tif" ))
}

tifs <- list.files("data/", pattern = "LC_", full.names = T)

rlist <- lapply(tifs,raster)
setMethod('mosaic', signature(x='list', y='missing'), 
          function(x, y, fun, tolerance=0.05, filename=""){
            stopifnot(missing(y))
            args <- x
            if (!missing(fun)) args$fun <- fun
            if (!missing(tolerance)) args$tolerance<- tolerance
            if (!missing(filename)) args$filename<- filename
            do.call(mosaic, args)
          })
mosaic(rlist, fun=mean) %>% writeRaster("data/usa_landcover_t1_2017.tif")

classification_table <- data.frame(
  name = c("Evergreen Needleleaf Forests", "Evergreen Broadleaf Forests",
          "Deciduous Needleleaf Forests","Deciduous Broadleaf Forests",
          "Mixed Forests","Closed Shrublands",
          "Open Shrublands","Woody Savannas",
          "Savannas","Grasslands",
          "Permanent Wetlands","Croplands",
          "Urban and Built-up Lands","Cropland/Natural  Vegetation  Mosaics",
          "Permanent Snow and Ice","Barren",
          "Water Bodies", "Unclassified"),
  value = c(1:17,255)
)
write_csv(classification_table,"data/usa_landcover_t1_classes.csv")
