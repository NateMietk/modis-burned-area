system("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/confusion_matrix/confusion_matrices_casted.csv data/confusion_matrices.csv")

confusing <- read_csv(file.path(version_dir, 'confusion_matrix', 'confusion_matrices_casted.csv')) %>%
  mutate(ratio = round(modisT_mtbsT/mtbsT_modisT_unique_modis_events, 4),
         ratios = abs(ratio-1) == min(abs(ratio-1)))

breaks <- c(min(confusing$ratio), 1, max(confusing$ratio))

p1 <- ggplot(confusing, aes(x = space, y = time)) +
  scale_color_manual(values = c(NA, "black")) +
  geom_tile(aes(fill = ratio, color = ratios), lwd = 1) +
  scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 1, breaks = breaks) +
  theme_pub()

ggsave(file = 'results/Optimum_ratio_mtbs_to_modis.pdf', p1, width = 5, height = 8, dpi=1200, scale = 3, units = "cm")
