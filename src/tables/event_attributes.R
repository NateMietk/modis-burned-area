# attribute table
libs<- c("tidyverse", "gridExtra", "sf")
lapply(libs, library, character.only = TRUE)

x = st_read("data/events_w_attributes.gpkg")

units <- c(NA, "pixels", "date", "day of year", "month", "year", "date", "days",
           "km2", "acres", "ha", "pixels/day", "km2/day", "acres/day", "ha/day")

event_level <- rep("event",15)

attribute_table <- data.frame(attribute = names(x)[1:length(names(x))-1],
                              units = units,
                              event_level = "yes",
                              daily_level = "no")

