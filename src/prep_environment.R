
# Download from:
# ftp://fuoco.geog.umd.edu/MCD64A1/C6/
# username: fire
# password: burnt

x <- c("MODIS", "tidyverse", "magrittr", "raster", "RCurl", 
       "gdalUtils", "foreach", "doParallel", "sf", "assertthat")
lapply(x, library, character.only = TRUE, verbose = TRUE)

# shapefiles --------------------------------
# input shapefile of interest
shp_dsn <- "src/es"          # this is the directory where the shapefile is stored
shp_layer <- "CBR" 
get_tiles <- function(dsn, layer){
  shp_dsn <- dsn          # this is the directory where the shapefile is stored
  shp_layer <- layer        # this is the pre-extension name of the files associated with the shapfile
  shp_oi <- st_read(dsn = shp_dsn,
                    layer = shp_layer, 
                    quiet= TRUE)
  
  # input modis shapefile
  ms_dsn <- "src/ms"
  ms_layer <- "modis_sinusoidal_grid_world"
  shp_ms <- st_read(dsn = ms_dsn,
                    layer = ms_layer, 
                    quiet= TRUE)
  
  tiles <- shp_oi %>%
    sf::st_transform(st_crs(shp_ms)) %>%
    st_intersection(shp_ms) %>%
    as.data.frame()%>%
    select(h, v) 
  tiles <- as.vector()
}

p4string_ea <- st_crs(shp_oi)
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
final_output <- file.path(top_directory, "result")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, modis_tiles_dir, MCD64A1_dir, hdf_months, tif_months, tif_year, final_output)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C6/"
u_p = "fire:burnt"

#Download the MODIS tile grid -------------------------
modis_shp <- file.path(modis_tiles_dir, "modis_sinusoidal_grid_world.shp")
if (!file.exists(modis_shp)) {
  loc <- "https://s3-us-west-2.amazonaws.com/modis-grids/modis_grid.zip"
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

modis_grid <- st_read(modis_shp, quiet= TRUE) %>%
  mutate(h = if_else(nchar(as.character(h)) == 1, paste0("h0", as.character(h)), paste0("h", as.character(h))),
         v = if_else(nchar(as.character(v)) == 1, paste0("v0", as.character(v)), paste0("v", as.character(v))),
         hv = paste0(h, v, sep = " "))

usa_shp <- st_read(us_shp, quiet= TRUE) %>%
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

