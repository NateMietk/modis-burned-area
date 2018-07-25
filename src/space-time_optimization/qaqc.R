system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/yearly_composites_15x15 data/stuff")

thing<-Sys.glob(paste("data/stuff/*.tif"))
for(i in thing){
  r <- raster(i)
  print(c(substr(r@file@name, nchar(r@file@name)-12,nchar(r@file@name)-4)
          , max(getValues(r))))
}