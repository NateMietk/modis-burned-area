fsr_df %>%
  ggplot(aes(x = year, y = spread_rate)) +
  geom_line() +
  facet_wrap(~ na_l1name) +
  theme_pub()

fsr_ecoreg <- fsr_df %>%
  group_by(na_l1name) %>%
  summarise(spread_rate = mean(spread_rate)) %>%
  left_join(ecoreg_slim, ., by = 'na_l1name')
plot(fsr_ecoreg['spread_rate'])


duration_df %>%
  ggplot(aes(x = year, y = duration)) +
  geom_line() +
  facet_wrap(~ na_l1name) +
  theme_pub()

duration_ecoreg <- duration_df %>%
  group_by(na_l1name) %>%
  summarise(duration = mean(duration)) %>%
  left_join(ecoreg_slim, ., by = 'na_l1name')
plot(duration_ecoreg['duration'])
