library(raster)

file<-"data/USA_BurnDate_2001.tif"
r <- raster(file)

year <- str_extract(file, "\\d{4}")
testday<-42

get_date_number<-function(year, day){
  return(
    as.numeric(as.Date(paste(year,day,sep="-"), "%Y-%j"))
         )
}

get_date_number(year, getValues(r)) %>% unique(
)

