devtools::install_github("NateMietk/MODISr")

x <- c("tidyverse", "magrittr", "raster", "RCurl", "gdalUtils", "foreach", "doParallel", "sf", "assertthat", 'lubridate', 'viridis', 'MODISr', 'lwgeom', 'scales')
lapply(x, library, character.only = TRUE, verbose = TRUE)

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
# p4string_ms <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")
ecoregion_prefix <- file.path(raw_prefix, "ecoregions")

# Output folders
MCD64A1_dir <- file.path(prefix, "MCD64A1")
version_dir <- file.path(MCD64A1_dir, "C6")
hdf_months <- file.path(version_dir, "hdf_months")
tif_months <- file.path(version_dir, "tif_months")
tif_year <- file.path(version_dir, "tif_years")
yearly_composites <- file.path(version_dir, "yearly_composites")
yearly_events <- file.path(version_dir, "yearly_events")

bounds_crt <- file.path(prefix, "bounds")
ecoreg_crt <- file.path(bounds_crt, "ecoregions")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, MCD64A1_dir, version_dir, mtbs_prefix,
                hdf_months, tif_months, tif_year, yearly_composites, ecoregion_prefix,
                bounds_crt, ecoreg_crt, ecoregion_out)
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
