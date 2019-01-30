# comparing areas between mtbs and modis

library(tidyverse)
library(ggpubr)

read_csv("tables/mtbs_modis_ids_ba_cast_s5t11.csv") -> d

output_table <- data.frame(max_area = NA,
                           r2 = NA)
nn <- 1000

for(i in 1:nn){
  e <- max(c(d$mtbs_hectares, d$modis_ha), na.rm = T)
  f <- (e/nn) * i
  g <- d[d$mtbs_hectares < f,]
  summary(lm(mtbs_hectares ~ modis_ha, g)) -> m
  output_table[i,1] <- f
  output_table[i,2] <- m$r.squared
}

p1 <- ggplot(d, aes(modis_ha, mtbs_hectares))+
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  theme_pubr() +
  geom_smooth(method = "lm", show.legend = T)

p2 <- ggplot(output_table, aes(max_area, r2)) +
  geom_line() +
  theme_pubr()

ggarrange(p1, p2, nrow = 2) +
  ggsave("area_analyse.pdf", dpi = 600, height = 8, width = 5)
