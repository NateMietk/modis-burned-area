# Fixed gdalUtils
# conda install gdal
# gdalinfo --version
# install.packages('gdalUtils', type = "source", configure.args=c(
#   '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config',
#   '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers',
#   '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

packages <- c("tidyverse", "magrittr", "raster", "RCurl", "gdalUtils", "foreach", "doParallel", "sf", 
       "assertthat", 'lubridate', 'viridis', 'scales', 'velox', 'ggmap', 'Hmisc', 'devtools',
       'classInt', 'RColorBrewer', 'rasterVis', 'RStoolbox', 'gridExtra', 'ggthemes', 'tidyr', 'broom')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
} else {
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
}

# devtools::install_github("NateMietk/FIREDr", force = TRUE)
# library(FIREDr)

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Raw data folders
prefix <- "data"
raw_prefix <- file.path(prefix, "raw")
ag_raw_dir <- file.path(raw_prefix, "US_140EVT_20180618")
us_prefix <- file.path(raw_prefix, "cb_2016_us_state_20m")
mtbs_prefix <- file.path(raw_prefix, "mtbs_fod_perimeter_data")
ecoregion_prefix <- file.path(raw_prefix, "ecoregions")
ecoregionl4_prefix <- file.path(raw_prefix, "us_eco_l4")

# Output folders
MCD64A1_dir <- file.path(prefix, "MCD64A1")
version_dir <- file.path(MCD64A1_dir, "C6")
hdf_months <- file.path(version_dir, "hdf_months")
tif_months <- file.path(version_dir, "tif_months")
yearly_mosaic <- file.path(version_dir, "yearly_mosaic")
monthly_mosaic <- file.path(version_dir, "monthly_mosaic")

yearly_events <- file.path(version_dir, "yearly_events")
stat_out <- file.path(version_dir, 'lvl1_eco_stats')

bounds_crt <- file.path(prefix, "bounds")
evt_dir <- file.path(bounds_crt, 'us_140evt')
ecoreg_crt <- file.path(bounds_crt, "ecoregions")
ecoregion_out <- file.path(ecoreg_crt, "us_eco_l3")
fire_dir <- file.path(prefix, 'fire')

s3_base <- 's3://earthlab-natem/modis-burned-area'

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(prefix, raw_prefix, us_prefix, MCD64A1_dir, version_dir, mtbs_prefix, ag_raw_dir, evt_dir,
                hdf_months, tif_months, yearly_mosaic, monthly_mosaic, ecoregion_prefix,
                ecoregionl4_prefix, bounds_crt, ecoreg_crt, ecoregion_out, fire_dir, stat_out)
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

theme_pub <- function(base_size=13, base_family="") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(hjust = 0.05, size = 13),
            
            panel.border = element_rect(colour = NA),
            panel.background = element_rect(colour = NA),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            plot.background = element_rect(colour = NA),
            
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            
            legend.title = element_text(size=14),
            legend.text = element_text(size=13),
            legend.direction = "vertical",
            legend.key = element_rect(colour = "transparent", fill = "transparent"),
            
            strip.background=element_rect(colour=NA),
            strip.text.x = element_text(size = 10),
            
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16)))
}

download_data <-  function(url, dir, layer, rp = raw_prefix) {
  dest <- paste0(rp, ".zip")
  
  if (!file.exists(layer)) {
    download.file(url, dest)
    unzip(dest, exdir = dir)
    unlink(dest)
    assert_that(file.exists(file.path(dir, paste0(layer, '.shp'))))
  }
}

