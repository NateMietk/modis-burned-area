# looking at day of year stuff

library(tidyverse)
library(sf)
library(ggplot2)
library(ggpubr)

d<-st_read("data/events_w_attributes_cus.gpkg") %>%
  mutate(ignition_doy = as.numeric(as.character(ignition_doy)))



dd <- read_csv("data/modis_burn_events_00_19.csv") %>%
  mutate(doy = strftime(date, format = "%j") %>% as.numeric)

ggarrange(
  ggplot(d, aes(x=ignition_doy)) +
    geom_histogram(binwidth = 1) +
    xlab("Day of Year") +
    scale_x_continuous(breaks=c(1,32,60,91,121,152,182,213,244,274,305,335,365))+
    ggtitle("Event Ignition Dates"),
  ggplot(dd, aes(x=doy)) +
    geom_histogram(binwidth = 1) +
    xlab("Day of Year") +
    scale_x_continuous(breaks=c(1,32,60,91,121,152,182,213,244,274,305,335,365))+
    ggtitle("All Burn Dates (i.e. every pixel)"),
ncol = 1, nrow = 2) +
  ggsave("images/day_of_year_historgrams.png", dpi=300, limitsize = TRUE)
