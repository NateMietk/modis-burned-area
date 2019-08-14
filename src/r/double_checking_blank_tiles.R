files <- list.files("data/scrap/tif_converted/", full.names = TRUE)

for (i in 1:length(files)){
x<-raster(files[i]) %>%
    getValues() %>%
    unique 
print(x)
}

files <- list.files("data/scrap/tif_months/", full.names = TRUE)
