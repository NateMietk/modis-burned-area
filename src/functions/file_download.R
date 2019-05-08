# Function to download files
file_download <- function(shp_path_name, shp_dir, url){
  if (!file.exists(shp_path_name)) {
    dest <- paste0(shp_dir, ".zip")
    download.file(url, dest)
    unzip(dest, exdir = shp_dir)
    unlink(dest)
    assert_that(file.exists(shp_path_name))
  }
}