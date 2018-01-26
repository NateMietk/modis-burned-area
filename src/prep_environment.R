
# Download from:
# ftp://fuoco.geog.umd.edu/MCD64A1/C6/
# username: fire
# password: burnt
x <- c("MODIS", "tidyverse", "magrittr", "raster", "RCurl", 
       "gdalUtils", "foreach", "doParallel", "sf", "assertthat")
lapply(x, library, character.only = TRUE, verbose = TRUE)

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"   #http://spatialreference.org/ref/sr-org/6903/
p4string_ms <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
modis_tiles_dir <- file.path(raw_prefix, "modis_grid")
# Output folders
MCD64A1_dir <- file.path(prefix, "MCD64A1")
top_directory <- file.path("data", "MCD64A1", "C6")
hdf_months <- file.path(top_directory, "hdf_months")
tif_months <- file.path(top_directory, "tif_months")
tif_year <- file.path(top_directory, "tif_years")
final_output <- file.path(top_directory, "usa_burndate")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, modis_tiles_dir, MCD64A1_dir, hdf_months, tif_months, tif_year, final_output)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

#Download the MODIS tile grid -------------------------
modis_shp <- file.path(modis_tiles_dir, "modis_sinusoidal_grid_world.shp")
if (!file.exists(modis_shp)) {
  loc <- "http://book.ecosens.org/wp-content/uploads/2016/06/modis_grid.zip"
  dest <- paste0(modis_tiles_dir, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = modis_tiles_dir)
  unlink(dest)
  assert_that(file.exists(modis_shp))
}

#Download the USA States layer -------------------------
us_shp <- file.path(us_prefix, "cb_2016_us_state_20m.shp")
if (!file.exists(us_shp)) {
  loc <- "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip"
  dest <- paste0(us_prefix, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = us_prefix)
  unlink(dest)
  assert_that(file.exists(us_shp))
}

usa_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  filter(!(STUSPS %in% c("AK", "HI", "PR"))) %>%
  dplyr::select(STUSPS) %>%
  st_transform(p4string_ea) 
names(usa_shp) %<>% tolower

# Reclassification matrix to remove NA values
mtrx = matrix(c(-Inf, 0, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
tiles = c("h08v04", "h08v05", "h09v04", "h09v05", "h10v04", "h10v05",
          "h10v06", "h11v04", "h11v05", "h12v04", "h12v05", "h13v04")

names <- c("BurnDate")
layers <- c("Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day")

#setup parallel backend to use many processors
cores <- detectCores()


usa_ms <- st_transform(usa_shp, crs = p4string_ms) %>%
  as(., "Spatial")


wus_shp <- st_read(dsn = us_prefix,
                   layer = "cb_2016_us_state_20m", quiet= TRUE) %>%
  filter(STUSPS %in% c("CO", "WA", "OR", "NV", "CA", "ID", "UT",
                       "WY", "NM", "AZ", "MT")) %>%
  dplyr::select(STUSPS) %>%
  st_transform(p4string_ea) 
names(wus_shp) %<>% tolower

wus_ms <- st_transform(wus_shp, crs = p4string_ms) %>%
  as(., "Spatial")
