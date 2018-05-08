source("src/a_prep_environment.R")

system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C5/hdf_months data/MCD64A1/C5/hdf_months")

# Download all .hdf files from ftp site for the Conterminous US
url = "ftp://fire:burnt@fuoco.geog.umd.edu/MCD64A1/C5/"
u_p = "fire:burnt"

# checking files after the fact and redownloading ----------------------------------
for (j in 1:length(tiles)){
  filenames <- RCurl::getURL(paste0(url, tiles[j],"/"), userpwd = u_p, v=T,
                             ftp.use.epsv = FALSE)

  # write to a temporary file
  cat(filenames, file = 'tmp.txt')

  # read the temporary file as a fixed width text file
  dir_listing <- read_fwf('tmp.txt', fwf_empty('tmp.txt'))

  # give columns good names
  names(dir_listing) <- c('z1', 'z2', 'z3', 'z4', 'size_in_bytes',
                          'month_modified', 'date_modified', 'year_modified',
                          'filename')

  # filter out columns we don't care about
  dir_listing <- dplyr::select(dir_listing, -starts_with('z'))

  # iterate over the files and download each
  for (i in 1:nrow(dir_listing)) {
    output_file_name <- file.path(hdf_months, dir_listing$filename[i])
    if (!file.exists(output_file_name)) {

      # check size of downloaded file
      local_size <- file.info(output_file_name)$size

      # check to see if the downloaded file size is identical to the servers file size
      are_bytes_identical <- identical(as.integer(local_size), dir_listing$size_in_bytes[i])

      # this might be better as a while loop...
      while (!are_bytes_identical) {
        # add warning if the downloaded file is a fragment of the source file
        tryCatch({download.file(paste0(url, tiles[j], "/", dir_listing$filename[i]),
                                output_file_name)}, silent = FALSE, condition = function(err) { } )
        local_size <- file.info(output_file_name)$size
        are_bytes_identical <- identical(as.integer(local_size), dir_listing$size_in_bytes[i])


      }
    }
  }
}

system("aws s3 sync  data/MCD64A1/C5/hdf_months s3://earthlab-natem/data/disturbances/fire/mcd64a1/hdf_months")


# https://stackoverflow.com/questions/31999808/retry-for-loop-r-loop-if-error
# for (i in 1:1000){
#   while(TRUE){
#     df <- try(downloadfnc("URL", file = i), silent=TRUE)
#     if(!is(df, 'try-error')) break
#   }
#   table[i,] <- df
# }

# https://stat.ethz.ch/pipermail/r-help/2007-October/143420.html
# tryCatch({download.file(url, destfile = file, quiet = FALSE, mode =
#                           "wb")}, silent = FALSE, condition = function(err) { } )
