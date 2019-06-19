# make yearly landcover mosaics from modis
library(httr)
library(tidyverse)
library(RCurl)
library(foreach)
library(doParallel)
# define tiles

tiles = c("h08v04","h09v04","h10v04","h11v04","h12v04","h13v04",
          "h08v05","h09v05","h10v05","h11v05","h12v05",
          "h08v06","h09v06","h10v06","h11v06")

years = 2001:2017

local_data <- "/home/a/data/MCD12Q1"

# input your username and password here as character strings
uu <- "admahood" 
pp <-

u_p <- paste0(uu,":",pp)

corz <- detectCores()-1
registerDoParallel(corz)
foreach (y = 1:length(years))%dopar%{
  dir.create(file.path(local_data, years[y]), showWarnings = F)
  
  url1 <-paste0("https://e4ftl01.cr.usgs.gov/MOTA/MCD12Q1.006/", years[y], ".01.01/")
  
  # "MCD12Q1.A2001001.h15v07.006.2018053201902.hdf"-> example
  # nchar(example)
  
  filenames <- RCurl::getURL(url1, userpwd = u_p)
  # write to a temporary file
  cat(filenames, file = 'tmp.txt')
  
  # read the temporary file as a fixed width text file
  dir_listing1 <- read_fwf('tmp.txt', fwf_empty('tmp.txt'),skip = 37) %>%
    dplyr::select(X1) %>%
    filter(str_detect(X1, "\\S{41}.hdf.xml"))%>%
    mutate(X1 = str_extract(X1, "\\S{41}.hdf")) %>%
    filter(substr(X1,18,23) %in% tiles)
  
  for(i in dir_listing1$X1){
    output_file_name <- file.path(local_data,years[y],i)
    if(!file.exists(output_file_name)){
      httr::GET(paste0(url1,i),
                authenticate(uu,pp),
                write_disk(path = output_file_name))
      }
  }
}

# checking to make sure they're all there
for(y in years){
  print(y)
print(length(list.files(file.path(local_data,y))))
}
