download_data <-  function(url, dir, layer, rp = raw_prefix) {
  dest <- paste0(rp, ".zip")
  
  if (!file.exists(layer)) {
    download.file(url, dest)
    unzip(dest, exdir = dir)
    unlink(dest)
    assert_that(file.exists(file.path(dir, paste0(layer, '.shp'))))
  }
}