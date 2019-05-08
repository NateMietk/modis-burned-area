
# Get NIFC reported numbers
nifc_df <- read_html('https://www.nifc.gov/fireInfo/fireInfo_stats_totalFires.html') %>%
  html_nodes("table") %>%
  html_table() %>%
  tibble(.) %>%
  unnest() %>%
  slice(4:62) %>%
  mutate(year = as.integer(X1),
         fire_freq = as.double(gsub(',', '', X2)),
         burn_area = as.double(gsub('\\*|,', '', X3))/247.105) %>%
  dplyr::select(year, fire_freq, burn_area) %>%
  filter(year >= 2001 & year <= 2016) %>%
  group_by() %>%
  summarise(nifc_n = sum(fire_freq),
            nifc_burn_area = sum(burn_area))

fired_ecoregion_df <- fired_ecoregion %>%
  as.data.frame() %>%
  dplyr::select(events, fired_burn_area, year, fsr, na_l1name) %>%
  distinct(events, .keep_all = TRUE) %>%
  as_tibble() %>%
  filter(year != 2017) %>%
  group_by(na_l1name) %>%
  summarise(fired_n = n(),
            fired_burn_area = sum(fired_burn_area)) %>%
  mutate(na_l1name = stringr::str_to_title(na_l1name))

mtbs_ecoregion_df <- mtbs_ecoreg %>%
  as.data.frame() %>%
  dplyr::select(fire_id, mtbs_burn_area, year = discovery_year, na_l1name) %>%
  distinct(fire_id, .keep_all = TRUE) %>%
  as_tibble() %>%
  group_by(na_l1name) %>%
  summarise(mtbs_n = n(),
            mtbs_burn_area = sum(mtbs_burn_area)) %>%
  mutate(na_l1name = stringr::str_to_title(na_l1name))

fired_mtbs_ecoregion <- mtbs_ecoregion_df %>%
  left_join(., fired_ecoregion_df, by = 'na_l1name')

fired_mtbs_ecoregion %>%
  write_csv(., file.path(draft_table_dir, 'level1_ecoregion_statistics_modis_v_fired.csv'))

# FSR 
fired_ecoregion_fsr_df <- fired_ecoregion %>%
  as.data.frame() %>%
  dplyr::select(events, fired_burn_area, year, fsr, na_l1name) %>%
  distinct(events, .keep_all = TRUE) %>%
  as_tibble() %>%
  group_by(na_l1name) %>%
  summarise(fired_n = n(),
            max_fsr = max(fsr),
            mean_fsr = mean(fsr, na.rm = TRUE),
            sd_fsr = sd(fsr, na.rm = TRUE)) %>%
  mutate(se_fsr = sd_fsr / sqrt(fired_n),
         lower_ci = mean_fsr - qt(1 - (0.05 / 2), fired_n - 1) * se_fsr,
         upper_ci = mean_fsr + qt(1 - (0.05 / 2), fired_n - 1) * se_fsr) %>%
  dplyr::select(na_l1name, fired_n, max_fsr, lower_ci, mean_fsr, upper_ci, sd_fsr, se_fsr) %>%
  mutate(na_l1name = stringr::str_to_title(na_l1name))

fired_ecoregion_fsr_df %>%
  write_csv(., file.path(draft_table_dir, 'level1_ecoregion_statistics_fsr_fired.csv'))



