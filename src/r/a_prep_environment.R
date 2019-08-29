# Fixed gdalUtils
# conda install gdal
# gdalinfo --version
# install.packages('gdalUtils', type = "source", configure.args=c(
#   '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config',
#   '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers',
#   '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

packages <- c("tidyverse", "magrittr", "raster", "RCurl", "gdalUtils", "foreach", "doParallel", "sf", 
       "assertthat", 'lubridate', 'viridis', 'scales', 'velox', 'ggmap', 'Hmisc', 'devtools', 'rvest',
       'classInt', 'RColorBrewer', 'rasterVis', 'RStoolbox', 'gridExtra', 'ggthemes', 'tidyr', 'broom')
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
} else {
  lapply(packages, library, character.only = TRUE, verbose = FALSE) 
}

# devtools::install_github("NateMietk/FIREDr", force = TRUE)
# library(FIREDr)

# load all functions
file_sources <- list.files(file.path('src', 'functions'), pattern="*.R", 
                           full.names=TRUE, ignore.case=TRUE)
invisible(sapply(file_sources, source, .GlobalEnv))

p4string_ea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

# Raw data folders
data_dir <- "data"
raw_dir <- file.path(data_dir, "raw")
raw_dir_ag <- file.path(raw_dir, "US_140EVT_20180618")
raw_dir_us <- file.path(raw_dir, "cb_2016_us_state_20m")
raw_dir_mtbs <- file.path(raw_dir, "mtbs")
raw_dir_ecoregion <- file.path(raw_dir, "ecoregions")
raw_dir_ecoregionl4 <- file.path(raw_dir, "us_eco_l4")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(data_dir, raw_dir, raw_dir_ag, raw_dir_us, raw_dir_mtbs, raw_dir_ecoregion, raw_dir_ecoregionl4)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# MODIS related folders
MCD64A1_dir <- file.path(data_dir, "MCD64A1")
C6_dir <- file.path(MCD64A1_dir, "C6")
hdf_months_dir <- file.path(C6_dir, "hdf_months")
tif_months_dir <- file.path(C6_dir, "tif_months")
yearly_mosaic_dir <- file.path(C6_dir, "yearly_mosaic")
monthly_mosaic_dir <- file.path(C6_dir, "monthly_mosaic")
yearly_events_dir <- file.path(C6_dir, "yearly_events")
yearly_stats_dir <- file.path(C6_dir, "yearly_events")

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(MCD64A1_dir, C6_dir, hdf_months_dir, tif_months_dir, yearly_mosaic_dir, 
                monthly_mosaic_dir, yearly_events_dir, yearly_stats_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

# Data output folders
bounds_dir <- file.path(data_dir, "bounds")
evt_dir <- file.path(bounds_dir, 'us_140evt')
ecoregion_dir <- file.path(bounds_dir, "ecoregions")
ecoregionl3_dir <- file.path(ecoregion_dir, "us_eco_l3")
fire_dir <- file.path(data_dir, 'fire')
mtbs_dir <- file.path(fire_dir, 'mtbs')
fired_dir <- file.path(fire_dir, 'fired')
fishnet_dir <- file.path(bounds_dir, 'fishnet')

# Check if directory exists for all variable aggregate outputs, if not then create
var_dir <- list(bounds_dir, evt_dir, ecoregion_dir, ecoregionl3_dir, fire_dir, fishnet_dir, mtbs_dir, fired_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))

s3_base <- 's3://earthlab-natem/modis-burned-area'

# Figure and Table directories
results_dir <- 'results'
draft_figs_dir <- file.path(results_dir, 'draft_figures')
draft_table_dir <- file.path(results_dir, 'draft_tables')

var_dir <- list(results_dir, draft_figs_dir, draft_table_dir)
lapply(var_dir, function(x) if(!dir.exists(x)) dir.create(x, showWarnings = FALSE))


