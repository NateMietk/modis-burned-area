#
# Handlers for bulk processing LANDFIRE data
#

#' An internal function that unpacks a landfire zipfile and reclassifies data
#' so that it is consistent with actual % cover or veg height.
#'
#' @param x full path to a LANDFIRE zipfile
unpackAndReclassifyLandfireZip <- function(x){
  zip_name <- unlist(strsplit(x,"/"))
  zip_name <- zip_name[length(zip_name)]
  zip_folders <- unlist(strsplit(zip_name,split="_"))
  zip_folders <- gsub(paste(zip_folders[2:3],collapse="_"),pattern=".zip",replacement="")
  
  system(paste("unzip -o ",x," -d /tmp",sep=""));
  
  if(grepl(x,pattern="EVH")){ # is this VEG HEIGHT data?
    cat(" -- calculating vegetation height for associations\n")
    
    veg_height <- raster::raster(paste("/tmp",toupper(zip_folders),tolower(zip_folders),sep="/"));
    
    grass_height <- veg_height
    grass_height[grass_height<101] <- 0
    grass_height[grass_height>103] <- 0
    grass_height <- ((2*(grass_height-100)*0.25))-0.25 # units are meters
    
    shrub_height <- veg_height
    shrub_height[shrub_height<104] <- 0
    shrub_height[shrub_height>107] <- 0
    shrub_height[shrub_height==104] <- 0.25
    shrub_height[shrub_height==105] <- 0.75
    shrub_height[shrub_height==106] <- 2
    shrub_height[shrub_height==107] <- 3.25
    
    tree_height <- veg_height
    tree_height[tree_height<108] <- 0
    tree_height[tree_height>111] <- 0
    tree_height[tree_height==108] <- 2.5
    tree_height[tree_height==109] <- 7.5
    tree_height[tree_height==110] <- 17.5
    tree_height[tree_height==111] <- 37.5
    
    return(list(grass_height,shrub_height,tree_height))
    
  } else if(grepl(x,pattern="EVC")) { # is this veg cover data?
    cat(" -- calculating vegetation % cover for associations\n")
    
    veg_cover <- raster::raster(paste("/tmp",toupper(zip_folders),tolower(zip_folders),sep="/"));
    
    grass_perc_cover <- veg_cover
    grass_perc_cover[grass_perc_cover < 121] <- 0
    grass_perc_cover[grass_perc_cover > 129] <- 0
    grass_perc_cover <- ((grass_perc_cover-120)*10)+5
    
    shrub_perc_cover <- veg_cover
    shrub_perc_cover[shrub_perc_cover < 111] <- 0
    shrub_perc_cover[shrub_perc_cover > 119] <- 0
    shrub_perc_cover <- ((shrub_perc_cover-110)*10)+5
    
    tree_perc_cover <- veg_cover
    tree_perc_cover[tree_perc_cover < 101] <- 0
    tree_perc_cover[tree_perc_cover > 109] <- 0
    tree_perc_cover <- ((tree_perc_cover-100)*10)+5
    
    return(list(grass_perc_cover,shrub_perc_cover,tree_perc_cover))
  }
  return(NULL)
}
#' An internal function that will merge an Nx3 dimensional
#' list of Landfire raster pieces into single surfaces.  The 3x refers to
#' a column value (1=grass,2=shrub,3=tree) to help parse the list-of-lists
#' returned by unpackAndReclassifyLandfireZip().  Normally, a user will never
#' see this function.
#'
#' @param x list-of-lists returned by unpackAndReclassifyLandfireZip()
#' @param column parent list in the list-of-lists e.g.(1,2,or 3) returned by
#' unpackAndReclassifyLandfireZip()
#' @param filename optional filename argument, passed to raster::merge()
merge_by <- function(x, column=NULL, filename=NULL){
  # fetch the i'th item from each list in our list-of-lists
  # e.g., x[1:4,i]
  focal <- lapply(x, FUN=function(x,i=NULL){ return(x[[i]]) }, i=column)
  raster::writeRaster(do.call(raster::merge,focal),filename=filename,overwrite=T)
}
#' Unpack and classify LANDFIRE veg % cover and height data for grass, shrub, and trees.
#'
#' @param x full path to directory containing LANDFIRE zipfiles
#' @export
processLandfireZip <- function(x){
  # process explanatory data for our grid points
  if(length(list.files(pattern="grass_|shrub_|tree_"))!=6){
    cover_zips <- list.files(x,pattern="EVC",full.names=T)
    height_zips <- list.files(x,pattern="EVH",full.names=T)
    # process vegetation cover and height data, as needed
    if(length(cover_zips)>0){
      cover <- vector('list',length(cover_zips))
      for(i in 1:length(cover_zips)){
        cover[[i]] <- habitatWorkbench:::unpackAndReclassifyLandfireZip(x=cover_zips[i])
      }
      # i.e., processLandfireZip() returns : grass_perc_cover, shrub_perc_cover, tree_perc_cover
      habitatWorkbench:::merge_by(cover,column=1,filename="grass_perc_cover.tif")
      habitatWorkbench:::merge_by(cover,column=2,filename="shrub_perc_cover.tif")
      habitatWorkbench:::merge_by(cover,column=3,filename="tree_perc_cover.tif")
      # clean-up
      rm(cover); gc()
    }
    if(length(height_zips)>0){
      height <- vector('list',length(height_zips))
      for(i in 1:length(height_zips)){
        height[[i]] <- habitatWorkbench:::unpackAndReclassifyLandfireZip(x=height_zips[i])
      }
      
      habitatWorkbench:::merge_by(height,column=1,filename="grass_height.tif")
      habitatWorkbench:::merge_by(height,column=2,filename="shrub_height.tif")
      habitatWorkbench:::merge_by(height,column=3,filename="tree_height.tif")
      
      rm(height); gc()
    }
  }
}