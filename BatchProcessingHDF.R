

# this script is run after removing all of the dots from the 
# modis filenames. from files downloaded from this website:
# ftp://fuoco.geog.umd.edu/db/MCD64A1/h08v05/
# username = fire pw = burnt
# change the substr() line to account for that if you don't do it
# there must not be any .hdf.xml type files in the directory..
# this will lead to a loop-stopping error. just deletete everything
# that is not an .hdf file from the working directory

library(gdalUtils)

workingdirectories = c("~/DATA/MODIS/h11v05",
                       "~/DATA/MODIS/h12v04",
                       "~/DATA/MODIS/h12v05",
                       "~/DATA/MODIS/h13v04")


for (d in 1:length(workingdirectories)) {
    
  workingdirectory = workingdirectories[d]

  setwd(workingdirectory)



  files = dir(pattern = ".hdf")

  # with dots deleted (this is a relic of an earlier method)
  # filename <- substr(files,9,21)
  # without dots deleted
  filename = paste(substr(files, 10,16), substr(files, 18, 23), sep="_")

  newfilename1 <- paste0("BurnDate", filename, ".tif")
  newfilename2 <- paste0("BurnDateUncertainty", filename, ".tif")
  newfilename3 <- paste0("QA", filename, ".tif")
  newfilename4 <- paste0("FirstDay", filename, ".tif")
  newfilename5 <- paste0("LastDay", filename, ".tif")

   for (i in 1:length(files)) {

    sds = get_subdatasets(files[i])
    gdal_translate(sds[1], dst_dataset = newfilename1[i])
    gdal_translate(sds[2], dst_dataset = newfilename2[i])
    gdal_translate(sds[3], dst_dataset = newfilename3[i])
    gdal_translate(sds[4], dst_dataset = newfilename4[i])
    gdal_translate(sds[5], dst_dataset = newfilename5[i])
    
    }
 }

# if this error happens, the hdf file is corrupt or something. redownload it!
#ERROR 4: Failed to open HDF4 file "MCD64A1A2002213h09v040512014205173806.hdf" for SDS reading.
#
#gdalinfo failed - unable to open 'MCD64A1A2002213h09v040512014205173806.hdf'.
#Error in split1[[1]] : subscript out of bounds
#In addition: Warning message:
#  running command '"/usr/bin/gdalinfo" "MCD64A1A2002213h09v040512014205173806.hdf"' had status 1 

