
sen <- function(..., weights = NULL) {
  mblm::mblm(...)
}

mean_fsr_p <- lvl1_eco_fsr_slim %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = reorder(na_l1name, -mean_fsr), y = mean_fsr)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = lower_95ci_fsr, ymax = upper_95ci_fsr), alpha=0.3) +
  xlab('') + ylab('Average fire rate of spread (pixels per day)') +
  theme_pub() 
ggsave(file = 'results/mean_fsr.pdf', mean_fsr_p, width = 6, height = 8, dpi=1200, scale = 3, units = "cm")

mean_fsr_ts_p <- lvl1_eco_fsr_ts %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = year, y = mean_fsr, group = na_l1name, color = na_l1name)) +
  # geom_line() +
  geom_smooth(method = sen, se = FALSE) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  xlab('') + ylab('Mean fire rate of spread (pixels per day)') +
  ggtitle('Theil Sen trend lines') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = 'none')
ggsave(file = 'results/mean_timeseries_fsr_theilsen.pdf', mean_fsr_ts_p, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")

max_fsr_p <- lvl1_eco_fsr_slim %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = reorder(na_l1name, -max_fsr), y = max_fsr)) +
  geom_bar(stat = 'identity') +
  xlab('') + ylab('Max fire rate of spread (pixels per day)') +
  theme_pub() 
ggsave(file = 'results/max_fsr.pdf', max_fsr_p, width = 6, height = 8, dpi=1200, scale = 3, units = "cm")

max_fsr_ts_p <- lvl1_eco_fsr_ts %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = year, y = max_fsr, group = na_l1name, color = na_l1name)) +
  # geom_line() +
  geom_smooth(method = sen, se = FALSE) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  xlab('') + ylab('Max fire rate of spread (pixels per day)') +
  ggtitle('Theil Sen trend lines') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = c(0.31,0.85))
ggsave(file = 'results/max_timeseries_fsr_theilsen.pdf', max_fsr_ts_p, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")

p1 <- as.data.frame(fsr_vs) %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = area_km2, y = fsr, group = na_l1name, color = na_l1name)) +
  geom_smooth(se = TRUE) +
  scale_color_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                                '#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')) +
  xlab('Fire size (km2)') + ylab('Fire spread rate (pixels per day)') +
  ggtitle('Generalized additive model') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = c(0.28,0.85))
ggsave(file = 'results/fsr_vs_area_timeseries_theilsen.pdf', p1, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")

p1 <- as.data.frame(fsr_vs) %>%
  filter(na_l1name != 'WATER') %>%
  ggplot(aes(x = duration, y = fsr)) +
  geom_smooth(se = TRUE) +
  scale_color_manual(values = c('#a6cee3')) +
  xlab('Fire size (km2)') + ylab('Fire spread rate (pixels per day)') +
  ggtitle('Generalized additive model') +
  labs(color = "Level 1 Ecoregion") +
  theme_pub() +
  theme(legend.position = 'none')
ggsave(file = 'results/fsr_vs_duration_timeseries_theilsen.pdf', p1, width = 8, height = 8, dpi=1200, scale = 3, units = "cm")
