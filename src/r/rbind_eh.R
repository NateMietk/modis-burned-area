#rbind eh

library(tidyverse)
# install.packages("snowfall")
# library(snowfall)
library(sf)

system("aws s3 sync s3://earthlab-natem/modis-burned-area/delineated_events/world/noedge data/noedge")

ne_files <- list.files("data/noedge", pattern = "gpkg", full.names=T)[65:195]

e_files <- list.files("data", pattern = "gpkg", full.names=T)

e_ne<-c(e_files, ne_files)

file_list<-list()

for(i in 1:length(e_ne)){
  file_list[[i]]<- st_read(e_ne[i])
}

rbinder <- function(..., cores=NULL){
  if(is.null(cores)){
    do.call("rbind", ...)
  }else{
    sequ <- as.integer(seq(1, length(...), length.out=cores+1))
    listOLists <- paste(paste("list", seq(cores), sep=""), " = ...[",  c(1, sequ[2:cores]+1), ":", sequ[2:(cores+1)], "]", sep="", collapse=", ") 
    dfs <- eval(parse(text=paste("list(", listOLists, ")")))
    suppressMessages(sfInit(parallel=TRUE, cores))
    dfs <- sfLapply(dfs, function(x) do.call("rbind", x))
    suppressMessages(sfStop())
    do.call("rbind", dfs)   
  }
}

# eh <- rbinder(file_list, cores = detectCores()-1) #didnt work
eh<- do.call("rbind", file_list) # takes about an hour
st_write(eh, "eastern_hemisphere_tomay2019.gpkg", delete_dsn = TRUE)
system("aws s3 cp eastern_hemisphere_tomay2019.gpkg s3://earthlab-natem/modis-burned-area/delineated_events/eastern_hemisphere_to_may2019.gpkg")
