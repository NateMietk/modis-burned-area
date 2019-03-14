library(tidyverse)

sr_file <- "data/ics209_allWFsitreps1999to2014.csv"
inc_file <- "data/ics209_allWFincidents1999to2014.csv"
inc <-read_csv(inc_file, guess_max = 500000)
sr <- read_csv(sr_file, guess_max = 500000)

big <- inc %>%
  filter(FINAL_ACRES > 400000) %>%
  dplyr::select(INCIDENT_ID) %>%
  mutate(way_big =TRUE)

ff <-  inc %>%
  filter(WF_MAX_FSR > 150000) %>%
  dplyr::select(INCIDENT_ID) %>%
  mutate(fast = TRUE)
  

fast_fires <- sr %>%
  left_join(ff) %>%
  filter(fast == TRUE) %>%
  mutate(event_day = ifelse(
    DISCOVERY_DOY > REPORT_DOY, 
    REPORT_DOY - DISCOVERY_DOY + 365, REPORT_DOY - DISCOVERY_DOY
  ))

way_big_fires <- sr %>% 
  left_join(big) %>%
  filter(way_big == TRUE) %>%
  mutate(event_day = ifelse(
    DISCOVERY_DOY > REPORT_DOY, 
    REPORT_DOY - DISCOVERY_DOY + 365, REPORT_DOY - DISCOVERY_DOY
  ))

# fastest fires
ggplot(fast_fires, aes(x=REPORT_DOY)) +
  geom_line(alpha = 0.85, aes(y = scale(TOTAL_PERSONNEL))) +
  geom_line(alpha = 0.85, color = "red",lty = 2, aes(y = scale(WF_FSR))) +
  facet_wrap(~INCIDENT_ID, scales = "free")

#biggest fires ------------------------------------------------------------
ggplot(way_big_fires, aes(x=REPORT_DOY)) +
  geom_line(alpha = 0.85, aes(y = TOTAL_PERSONNEL)) +
  geom_line(alpha = 0.85, color = "red",lty = 2, aes(y = scale(WF_FSR)*500)) +
  facet_wrap(~INCIDENT_ID, scales = "free")

