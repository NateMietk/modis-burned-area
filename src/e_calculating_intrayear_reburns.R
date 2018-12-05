# script for calculating reburns within years
source("src/a_prep_environment.R")
system("aws s3 sync s3://earthlab-natem/modis-burned-area/MCD64A1/C6/tif_months data/MCD64A1/C6/tif_months")

#reclassifying to binary
mtrx <- matrix(c(-Inf, 0.5, 0, 368, Inf, 0, .5,368,1), byrow=TRUE, ncol=3)

years <- 2001:2017
# Create yearly composites for all tiles

for (j in 1:length(tiles)){
  for(i in 1:length(years)) {
    require(magrittr)
    require(raster)
    
    tile_files = as.vector(Sys.glob(paste0(tif_months, "/", "*", years[i], "*", tiles[j], ".tif")))
    
    pb <- txtProgressBar(min = 0, max = length(tile_files)*length(tiles), style = 3)
    
    if(!file.exists(paste(tif_year, "/Yearly_n_", tiles[j], "_", years[i], ".tif", sep=""))){
      fire = raster::stack(tile_files) %>%
        raster::reclassify(., mtrx) %>%
        raster::calc(., sum)
      
      tfilename = paste(tif_year, "/Yearly_n_", tiles[j], "_", years[i], ".tif", sep="")
      
      raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
    }
    setTxtProgressBar(pb, i)
  }
}

# then tally the numbers
rasts <- list.files(tif_year)

results <- data.frame(observation = NA, reburn_pct = NA, n2_or_more = NA, n1=NA)
counter = 1
for(i in 1:length(rasts)){
  print(i/length(rasts))
  x <- raster(paste0("data/MCD64A1/C6/tif_years/",rasts[i]))
  y <- getValues(x)
  z <- as.data.frame(table(y))
  
  if (nrow(z)>2){
    results[counter,1] <- rasts[i]
    results[counter,2] <- round(sum(z$Freq[c(3:nrow(z))])/z$Freq[2]*100,2)
    results[counter,3] <- sum(z$Freq[c(3:nrow(z))])
    results[counter,4] <- z$Freq[2]
    counter <- counter +1
  }else{print("nada")}
}

write.csv(results,"data/intrayear_reburns.csv")
system("aws s3 cp data/intrayear_reburns.csv s3://earthlab-natem/modis-burned-area/intrayear_reburns.csv")
