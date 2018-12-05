
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}
tmp <- fsr_ecoreg_pts %>%
  group_by(year) %>%
  summarise(mfsr = mean(fsr, na.rm = TRUE))
  
mean_fsr_p <- fsr_ecoreg_pts %>%
  ggplot(aes(x = as.integer(as.character(year)), y = fsr)) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 3)) +
  geom_point(data = tmp, aes(y = mfsr)) +
  geom_line(data = tmp, aes(y = mfsr)) +
  xlab('') + ylab('Average fire rate of spread (pixels per day)') +
  theme_pub() 
ggsave(file = 'results/time_series_mean_fsr.pdf', mean_fsr_p, width = 8, height = 8, dpi=1200, scale = 2, units = "cm")

mean_fsr_p <- fsr_ecoreg_pts %>%
  na.omit() %>%
  transform(region = factor(region, levels=c("East", "Central", 'West'))) %>%
  ggplot(aes(x = as.integer(as.character(year)), y = fsr, group = region, color = region)) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 4)) +
  scale_color_brewer('Event ID', palette = 'BrBG', na.value = NA) +
  xlab('') + ylab('Average fire rate of spread (pixels per day)') +
  theme_pub() +
  theme(legend.position = 'top',
        legend.direction = "horizontal")
ggsave(file = 'results/regional_time_series_mean_fsr.pdf', mean_fsr_p, width = 8, height = 8, dpi=1200, scale = 2, units = "cm")

mean_fsr_p <- lvl1_eco_fsr_slim %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = reorder(na_l1name, -mean_fsr), y = mean_fsr)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_95ci_fsr, ymax = upper_95ci_fsr), width = 0.5) +
  coord_flip() +
  xlab('') + ylab('Average fire rate of spread (pixels per day)') +
  theme_pub() 
ggsave(file = 'results/mean_fsr.pdf', mean_fsr_p, width = 10, height = 6, dpi=1200, scale = 3, units = "cm")

p1 <- as.data.frame(fsr_vs) %>%
  transform(region = factor(region, levels=c("East", "Central", 'West'))) %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = year, y = duration, group = region, color = region)) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 3)) +
  scale_color_brewer('Regions', palette = 'BrBG', na.value = NA) +
  xlab('Year') + ylab('Duration (# days)') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = 'top',
        legend.direction = "horizontal",
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1))
ggsave(file = 'results/region_duration_timeseries.pdf', p1, width = 8, height = 8, dpi=1200, scale = 2, units = "cm")


p1 <- as.data.frame(fsr_vs) %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = year, y = duration, group = na_l1name, color = na_l1name)) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 3), se = FALSE) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  xlab('Year') + ylab('Duration (# days)') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(axis.text.x = element_text(size = 16, angle = 45, hjust = 1))
ggsave(file = 'results/l1ecoregion_duration_timeseries.pdf', p1, width = 12, height = 8, dpi=1200, scale = 2, units = "cm")


p1 <- as.data.frame(fsr_vs) %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = area_km2, y = fsr, group = na_l1name, color = na_l1name)) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 4)) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  xlab('Fire size (km2)') + ylab('Fire spread rate (pixels per day)') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = c(0.28,0.85),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1))
ggsave(file = 'results/fsr_vs_area_timeseries_theilsen.pdf', p1, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")

p1 <- as.data.frame(fsr_vs) %>%
  filter(!is.na(region)) %>%
  transform(region = factor(region, levels=c("East", "Central", 'West'))) %>%
  ggplot(aes(x = duration, y = fsr, group = region, color = region), na.value = NA) +
  geom_smooth(method = 'lm', formula = y ~ splines::bs(x, 4), na.value = NA) +
  scale_color_brewer('Event ID', palette = 'BrBG', na.value = NA) +
  xlab('Fire size (km2)') + ylab('Fire spread rate (pixels per day)') +
  # ggtitle('Generalized additive model') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = 'top',
        legend.direction = "horizontal")
ggsave(file = 'results/fsr_vs_duration_timeseries_theilsen.pdf', p1, width = 8, height = 8, dpi=1200, scale = 2, units = "cm")


t1 <- as.data.frame(fsr_ecoreg_pts) %>%
  filter(na_l1name != 'WATER') %>%
  left_join(., ecoregl1, by = 'na_l1name') %>%
  dplyr::select(year, fsr) %>%
  mutate(year = as.integer(as.character(year)))

t1 <- t1 %>%
  ggplot(aes(x = year, y = log(fsr))) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 1)) 
ggsave(file = 'results/fsr_mean_timeseries_global.pdf', t1, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")


t1 <- as.data.frame(fsr_ecoreg_pts) %>%
  filter(na_l1name != 'WATER') %>%
  left_join(., ecoregl1, by = 'na_l1name') %>%
  dplyr::select(region, year, fsr) %>%
  mutate(year = as.integer(as.character(year)))

t1 <- t1 %>%
  ggplot(aes(x = year, y = log(fsr), group = region, color = region)) +
  geom_point() +
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 1)) 
ggsave(file = 'results/fsr_mean_timeseries_regional.pdf', t1, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")
  