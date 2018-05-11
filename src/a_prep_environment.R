
x <- c("tidyverse", "magrittr", "raster", "RCurl", "gdalUtils", "foreach", "doParallel", "sf", "assertthat", 'lubridate')
lapply(x, library, character.only = TRUE, verbose = TRUE)

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
p4string_ms <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")

# Output folders
MCD64A1_dir <- file.path(prefix, "MCD64A1")
version_dir <- file.path(MCD64A1_dir, "C6")
hdf_months <- file.path(version_dir, "hdf_months")
tif_months <- file.path(version_dir, "tif_months")
tif_year <- file.path(version_dir, "tif_years")
yearly_composites <- file.path(version_dir, "yearly_composites")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, MCD64A1_dir, version_dir, mtbs_prefix,
                hdf_months, tif_months, tif_year, yearly_composites)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Function to download files
file.download <- function(shp_path_name, shp_dir, url){
  if (!file.exists(shp_path_name)) {
    dest <- paste0(shp_dir, ".zip")
    download.file(url, dest)
    unzip(dest, exdir = shp_dir)
    unlink(dest)
    assert_that(file.exists(shp_path_name))
  }
}

# Download the USA States layer -------------------------
file.download(file.path(us_prefix, "cb_2016_us_state_20m.shp"),
              us_prefix, "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip")

# Import and prep the USA shapefile
usa <- st_read(file.path(us_prefix, "cb_2016_us_state_20m.shp"),
               quiet= TRUE) %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR"))) %>%
  dplyr::select(STUSPS) %>%
  st_transform(p4string_ea)
names(usa) %<>% tolower

# Reproject USA shapefile to MODIS sinusoidal
usa_ms <- st_transform(usa, crs = p4string_ms) %>%
  as(., "Spatial")

# Import and prep the USA shapefile and extract for only the Western US
wus <- st_read(file.path(us_prefix, "cb_2016_us_state_20m.shp"),
                   quiet= TRUE) %>%
  filter(STUSPS %in% c("CO", "WA", "OR", "NV", "CA", "ID", "UT",
                       "WY", "NM", "AZ", "MT")) %>%
  dplyr::select(STUSPS) %>%
  st_transform(p4string_ea)
names(wus) %<>% tolower

# Reproject WUS shapefile to MODIS sinusoidal
wus_ms <- st_transform(wus, crs = p4string_ms) %>%
  as(., "Spatial")

# This function extracts the MODIS tiles that intersect with the shapefile area of interest
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

# Vector of MODIS tiles to download
tiles <- get_tiles(usa)

files <- c('raw', 'cleaned', 'filled')
names <- c("BurnDate")
layers <- c("Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day")
