library(tidyverse)
library(googledrive)

#fetch data from adam's google drive if necessary

drive_find("/_current_projects/sr209/",n_max = 5) ->x

sr_file <- "data/ics209_allWFsitreps1999to2014.csv"
inc_file <- "data/ics209_allWFincidents1999to2014.csv"
inc <-read_csv(inc_file, guess_max = 500000)
sr <- read_csv(sr_file, guess_max = 500000)

big <- inc %>%
  filter(FINAL_ACRES > 363000) %>%
  dplyr::select(INCIDENT_ID) %>%
  mutate(way_big =TRUE);dim(big)

ff <-  inc %>%
  filter(WF_MAX_FSR > 145000) %>%
  dplyr::select(INCIDENT_ID) %>%
  mutate(fast = TRUE);dim(ff)



fast_fires_1 <- sr %>%
  group_by(INCIDENT_ID, REPORT_DOY) %>%
  summarise(TOTAL_PERSONNEL = sum(TOTAL_PERSONNEL, na.rm=T),
            PCT_FINAL_SIZE = mean(PCT_FINAL_SIZE, na.rm=T),
            WF_FSR = max(WF_FSR, na.rm=T),
            STR_THREATENED = sum(STR_THREATENED, na.rm=T)) %>%
  ungroup()

fixed_inc_stats <- fast_fires_1 %>%
  group_by(INCIDENT_ID) %>%
  summarise(STR_THREATENED_MAX = max(STR_THREATENED, na.rm=T), 
            WF_PEAK_PERSONNEL = max(TOTAL_PERSONNEL, na.rm=T)) %>%
  ungroup()

  
fast_fires <- fast_fires_1 %>%
  left_join(ff) %>%
  left_join(fixed_inc_stats, by="INCIDENT_ID") %>%
  left_join(dplyr::select(inc, INCIDENT_ID, WF_MAX_FSR,# STR_THREATENED_MAX, WF_PEAK_PERSONNEL, 
                          FINAL_ACRES), 
            by = "INCIDENT_ID") %>%
  filter(fast == TRUE) %>%
  mutate(adj_fsr = WF_FSR/WF_MAX_FSR,
         adj_personnel = TOTAL_PERSONNEL/WF_PEAK_PERSONNEL,
         adj_str = STR_THREATENED/STR_THREATENED_MAX) %>%
  dplyr::select(REPORT_DOY,INCIDENT_ID,
                Total_Personnel = adj_personnel, 
                Fire_Spread_Rate = adj_fsr,
                Structures_Threatened =adj_str, 
                Cumulative_Area_Burned = PCT_FINAL_SIZE)  %>%
  gather(Variable, Value, -INCIDENT_ID, -REPORT_DOY)

way_big_fires <- fast_fires_1 %>% 
  left_join(big) %>%
  left_join(fixed_inc_stats, by="INCIDENT_ID") %>%
  left_join(dplyr::select(inc, INCIDENT_ID, WF_MAX_FSR#,
                         # WF_PEAK_PERSONNEL,STR_THREATENED_MAX
                         ), 
            by = "INCIDENT_ID") %>%
  filter(way_big == TRUE) %>%
  mutate(adj_fsr = WF_FSR/WF_MAX_FSR,
         adj_personnel = TOTAL_PERSONNEL/WF_PEAK_PERSONNEL,
         adj_str = STR_THREATENED/STR_THREATENED_MAX) %>%
  dplyr::select(REPORT_DOY,INCIDENT_ID,
                Total_Personnel = adj_personnel, 
                Fire_Spread_Rate = adj_fsr,
                Structures_Threatened =adj_str,
                Cumulative_Area_Burned = PCT_FINAL_SIZE) %>%
  gather(Variable, Value, -INCIDENT_ID, -REPORT_DOY)

# fastest fires
ggplot(fast_fires, aes(x=REPORT_DOY,y=Value, color = Variable)) +
  geom_line(alpha = 0.85, aes(lty = Variable)) +
  facet_wrap(~INCIDENT_ID, scales = "free") +
  ggtitle("Fastest Fires") +
  theme_bw()+
  ggsave("images/fast_fires_fsr.pdf")

#biggest fires ------------------------------------------------------------
ggplot(way_big_fires, aes(x=REPORT_DOY,y=Value, color = Variable)) +
  geom_line(alpha = 0.85, aes(lty = Variable)) +
  facet_wrap(~INCIDENT_ID, scales = "free") +
  theme_bw()+
  ggtitle("Largest Fires") +
  ggsave("images/big_fires_fsr.pdf")

# exploring total personnel issues in complexes

valley <- filter(sr, INCIDENT_ID == "2000_MT-BRF-11445_VALLEY COMPLEX")
