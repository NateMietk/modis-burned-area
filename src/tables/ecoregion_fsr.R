# calculating fire spread rate stuff for the table in the paper

libs <- c("sf", "tidyverse")
lapply(libs, library, character.only = TRUE)

events <- st_read("data/events_w_attributes.gpkg") %>%
  st_set_geometry(NULL) %>%
  mutate(ignition_year = as.numeric(as.character(ignition_year))) %>%
  #na.omit() %>%
  filter(ignition_year > 2000 & ignition_year < 2017) %>%
  group_by(l1_ecoregion) %>%
  summarize(events = n(),
            max_fsr_ha = max(fsr_ha_per_day),
            min_fsr_ha = min(fsr_ha_per_day),
            mean_fsr_ha = mean(fsr_ha_per_day),
            five_pct = quantile(fsr_ha_per_day, probs = 0.05),
            ninetyfive_pct = quantile(fsr_ha_per_day, probs = 0.95),
            std_dev = sd(fsr_ha_per_day),
            std_err = sd(fsr_ha_per_day)/sqrt(n()))

write_csv(events[1:10,], "src/csv_files/fsr_table.csv")
