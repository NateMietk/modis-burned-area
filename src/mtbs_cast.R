# a working script to figure out how to split up polygons for the confusion matrix par script

mtbs_cast <- st_cast(mtbs[0], to = "POLYGON")

mtbs_cast$duped <- duplicated(mtbs_cast$Fire_ID)

mtbs_cast$new_id <- ifelse(mtbs_cast$duped == TRUE,
                           paste(as.character(mtbs_cast$Fire_ID),as.character(row_number(mtbs_cast$Fire_ID)), sep="_"),
                           as.character(mtbs_cast$Fire_ID))

mtbs_cast$cast_area_ha <- st_area(mtbs_cast[0])%>% set_units(value = hectare)
mtbs_cast$cast_area_ac <- st_area(mtbs_cast[0])%>% set_units(value = acre)
