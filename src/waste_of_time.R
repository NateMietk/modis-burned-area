# files <- list.files("data/yearly_composites_15x15/s1t1/")
# files5 <- list.files("data/yearly_composites_15x15/s5t5/")
# 
# for(i in files5){
#   r <- raster(file.path("data/yearly_composites_15x15/s5t5",i))
#   print(max(getValues(r)))
# }
# 


# Create data
r <- raster("data/yearly_composites_15x15/s1t1/USA_BurnDate_2001s1t1.tif")
r[r<1] <- NA
r.pts <- rasterToPoints(r, spatial=TRUE)
geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
r.pts <- spTransform(r.pts, CRS(geo.prj))
r.pts <- data.frame(r.pts@data, long=coordinates(r.pts)[,1]) %>%
  as_tibble() %>%
  group_by(USA_BurnDate_2001s1t1) %>%
  summarize(pixel_count = n(),
            long = mean(long))


r.pts <- spTransform(r.pts,(crs(r)))

r.pts$w_e <- ifelse(r.pts$long < -97, 1,0)
r.pts$USA_BurnDate_2001s1t1 = NULL
r.pts$long = NULL
r.pts$lat = NULL

rasterize(r.pts, r, field = r.pts$w_e, filename = "data/w.tif")

# Assign coordinates to @data slot, display first 6 rows of data.frame
                       
# head(r.pts@data)

thanks_internet <- function(x, a, filename) {
  # this function multiplies each cell of a raster by a
  # x: the raster, either a character string or already rastered in object
  # a: object to multiply each cell by
  # filename: if you don't already know please return my stolen computer
  out <- raster(x)
  out <- writeStart(out, filename, overwrite=TRUE)
  for (r in 1:nrow(out)) {
    v <- getValues(x, r)
    v <- v * a
    out <- writeValues(out, v, r)
  }
  out <- writeStop(out)
  return(out)
}




