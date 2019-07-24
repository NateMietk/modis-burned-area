# Import and prep the USA shapefile
if (!exists("states")){
  download_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                dir = raw_dir_us,
                layer = "cb_2016_us_state_20m") 
  
  states <- st_read(file.path(raw_dir_us, "cb_2016_us_state_20m.shp")) %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

# world shapefile

# if (!exists("world")){
#   download_data(url = "http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip",
#                 dir = raw_dir_us,
#                 layer = "TM_WORLD_BORDERS_SIMPL-0.3") 
#   
#   states <- st_read(file.path(raw_dir_us, "TM_WORLD_BORDERS_SIMPL-0.3.shp")) #%>%
#     #sf::st_transform(p4string_ea) %>%
#     #dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
#   #states$STUSPS <- droplevels(states$STUSPS)
# }

# Vector of MODIS tiles to download
tiles <- FIREDr::get_tiles(states)

# Download all HDF files from 2001-2017 for the CONUS
FIREDr::download_tiles(tiles, out_dir = hdf_months)

# Convert the raw HDF to GeoTifs
FIREDr::convert_hdf_tif(tiles, in_dir = hdf_months, out_dir = tif_months, n_cores = parallel::detectCores(), layer_names = "BurnDate")

FIREDr::mosaic_tiles(n_cores = 8,  in_dir = tif_months, 
                     out_dir = monthly_mosaic, timescale = 'months')

system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
