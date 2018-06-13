system("aws s3 cp s3://earthlab-natem/modis-burned-area/MCD64A1/C6/confusion_matrix/confusion_matrices_casted.csv data/confusion_matrices.csv")
library(ggplot2)
library(viridis)
library(scales)

x <- read.csv("data/confusion_matrices.csv", stringsAsFactors = FALSE)

x$sum_means <- x$mean_n_modis_per_mtbs + x$mean_n_mtbs_per_modis
x$ismin <- x$sum_means == min(x$sum_means)

ggplot(x, aes(x = space, y = time)) +
  scale_color_manual(values = c(NA, "black")) +
  geom_tile(aes(fill = sum_means, color=ismin), lwd=1) +
  scale_fill_viridis(direction = -1, values=rescale(c(1,1.1,5))) +
  theme_bw()

ggplot(x, aes(x = space, y = time)) +
  scale_color_manual(values = c(NA, "black")) +
  geom_tile(aes(fill = mean_n_modis_per_mtbs, color=ismin), lwd=1) +
  scale_fill_viridis(direction = -1, values=rescale(c(1,1.1,5))) +
  theme_bw()

ggplot(x, aes(x = space, y = time)) +
  scale_color_manual(values = c(NA, "black")) +
  geom_tile(aes(fill = mean_n_mtbs_per_modis, color=ismin), lwd=1) +
  scale_fill_viridis(direction = -1) +
  theme_bw()
