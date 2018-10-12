
shape_crop <- rim_ms
  
# import all fire event tifs into raster stack
for (i in 2010:2013) {
  
  outname <- file.path(version_dir, 'vector_out', paste0('USA_Vector_Events_', i, '.gpkg'))
                       
  event_list <- list.files(file.path(version_dir, 'yearly_events'),
                           full.names = TRUE, pattern = paste0(i, '_ms'))

  composite_list <- list.files(file.path(version_dir, 'yearly_composites'),
                             full.names = TRUE, pattern = paste0(i, '_ms'))
  file_split <- event_list %>%
    basename %>%
    strsplit(split = "_") %>%
    unlist
  year_split <- file_split[3] 
  
  composite <- raster(composite_list) 
  composite[composite < 1] <- NA
  
  events <- raster(event_list) 
  events[events < 1] <- NA
  
  combined = brick(composite, events)
  combined@data@attributes[1][[1]] <- values(combined)

  sf_rst <- rasterToPolygons(combined, dissolve= TRUE) %>%
    st_as_sf() %>%
    group_by(paste0('USA_BurnDate_', i, '_ms'), paste0('USA_burnevents_', i, '_ms')) %>%
    summarise(pixel_count = n()) %>%
    mutate(year = year_split,
           event_id = paste0(year, '_', paste0('USA_burnevents_', i, '_ms')),
           burn_date_id = paste0(event_id, '_', paste0('USA_BurnDate_', i, '_ms')),
           burn_date = paste0('USA_BurnDate_', i, '_ms')) %>%
    ungroup() %>%
    dplyr::select(event_id, burn_date_id, pixel_count, year, burn_date)
  st_write(sf_rst, outname, driver = 'GPKG')
}

