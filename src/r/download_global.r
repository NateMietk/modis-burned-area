dl_stuff<-function (tiles, url = "ftp://fuoco.geog.umd.edu/MCD64A1/C6/", 
          u_p = "fire:burnt", out_dir) 
{
  u_p_url <- paste0("ftp://", u_p, "@fuoco.geog.umd.edu/MCD64A1/C6/")
  requireNamespace("tidyverse")
  requireNamespace("RCurl")
  for (j in 1:length(tiles)) {
    filenames <- RCurl::getURL(paste0(url, tiles[j], "/"), 
                               userpwd = u_p, v = T, ftp.use.epsv = FALSE)
    cat(filenames, file = "tmp.txt")
    dir_listing <- read_fwf("tmp.txt", fwf_empty("tmp.txt"))
    names(dir_listing) <- c("z1", "z2", "z3", "z4", "size_in_bytes", 
                            "month_modified", "date_modified", "year_modified", 
                            "filename")
    dir_listing <- dplyr::select(dir_listing, -starts_with("z"))%>%
      filter(substr(filename,1,7) == "MCD64A1")
    for (i in 1:nrow(dir_listing)) {
      output_file_name <- file.path(out_dir, dir_listing$filename[i])
      if (!file.exists(output_file_name)) {
        # download.file(paste0(u_p_url, tiles[j], "/", dir_listing$filename[i]), 
        #               output_file_name)
        uu<-strsplit(u_p, ":")[[1]][1]
        pp<-strsplit(u_p, ":")[[1]][2]
        httr::GET(paste0(url,tiles[j],"/",dir_listing$filename[i]),
                  authenticate(uu,pp),
                  write_disk(path = output_file_name,overwrite =TRUE)
                  )
        
        local_size <- file.info(output_file_name)$size
        are_bytes_identical <- identical(as.integer(local_size), 
                                         dir_listing$size_in_bytes[i])
        if (!are_bytes_identical) {
          warning(paste("Mismatch in file size found for", 
                        dir_listing$filename[i]))
          unlink(dir_listing$filename[i])
        }
      }
    }
  }
  unlink("tmp.txt")
}

# actual script ===========================
url1 <- "ftp://fuoco.geog.umd.edu/MCD64A1/C6/"
u_p <- "fire:burnt"

filenames <- RCurl::getURL(url1, 
                           userpwd = u_p, v = T, ftp.use.epsv = FALSE)
cat(filenames, file = "tmp.txt")
tiles <- read_fwf("tmp.txt", fwf_empty("tmp.txt"))[4:271,9]$X9
dir.create("hdfs")
dl_stuff(tiles=tiles, url=url1, u_p = u_p, out_dir = "hdfs")
