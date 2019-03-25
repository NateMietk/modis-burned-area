# Import and prep the USA shapefile
if (!exists("states")){
  download_data(url = "https://www2.census.gov/geo/tiger/GENZ2016/shp/cb_2016_us_state_20m.zip",
                dir = us_prefix,
                layer = "cb_2016_us_state_20m") 
  
  states <- st_read(file.path(us_prefix, "cb_2016_us_state_20m.shp")) %>%
    sf::st_transform(p4string_ea) %>%
    dplyr::filter(!STUSPS %in% c("HI", "AK", "PR"))
  states$STUSPS <- droplevels(states$STUSPS)
}

# Vector of MODIS tiles to download
tiles <- FIREDr::get_tiles(states)

# Download all HDF files from 2001-2017 for the CONUS
FIREDr::download_tiles(tiles, out_dir = hdf_months)

# Convert the raw HDF to GeoTifs
FIREDr::convert_hdf_tif(tiles, in_dir = hdf_months, out_dir = tif_months, n_cores = parallel::detectCores(), layer_names = "BurnDate")

FIREDr::mosaic_tiles(n_cores = parallel::detectCores()/2,  in_dir = tif_months, out_dir = yearly_composites, mask = states, timescale = 'months')









# Create yearly raster based on the max burned date of each month
FIREDr::aggreate_to_yearly(tiles, n_cores = parallel::detectCores(), year_range = '2001:2017', 
                   in_dir = tif_months = tif_year, out_dir)

# Mosaic the yearly tiles to yearly CONUS rasters
FIREDr::mosaic_tiles(year_range = '2001:2017', n_cores = parallel::detectCores()/2,  in_dir = tif_months, out_dir = yearly_composites, 
             mask = usa, to_project = TRUE, projection = p4string_ea, projected_resolution = '500', projection_method = 'ngb')

system("aws s3 sync data/MCD64A1/C6 s3://earthlab-natem/modis-burned-area/MCD64A1/C6")
