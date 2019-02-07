# comparing areas between mtbs and modis

library(tidyverse)
library(ggpubr)
library(scales)

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

output_table$max_area <- output_table$max_area/e



output_table$pred <- predict(lm(r2~poly(max_area,20), output_table))

ss = 2
for(i in 1:nrow(output_table)){
  rise <- output_table$pred[i+1] - output_table$pred[i]
  run <- output_table$max_area[i+1] - output_table$max_area[i]
  rr <- rise/run
  print(rr)
  if(rr < ss && rr > 0.99999999999999999 && is.na(rr) == FALSE){ss <- rr; 
  tt <- output_table$max_area[i]}
}
rrr<-paste("R^2 == ",round(m$r.squared,2))
p1 <- ggplot(d, aes(modis_ha, mtbs_hectares))+
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.15) +
  geom_abline(slope=1, intercept=0) +
  theme_pubr() +
  
  coord_fixed() +
  xlab("MODIS (hectares)") +
  ylab("MTBS (hectares)") +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+
  annotate("text",  x=150000, y=130000, label = "1:1 line") +
  annotate("text",  x=100000, y=150000, col="blue", 
           label = rrr, parse=T)


p2 <- ggplot(output_table, aes(max_area, r2)) +
  geom_vline(aes(xintercept=tt), col="grey20", lty=2)+
  geom_line() +
  geom_line(aes(y= pred), col="red")+
  coord_fixed()+
  theme_pubr() +
  xlab("Max Area (standardized)")+
  ylab(expression(R^2)) +
  annotate("text",  x=0.28, y=tt+0.2, label = paste("Slope = 1 at", round(tt*e), "hectares"))

ggarrange(p1, p2, nrow = 1, ncol=2) +
  ggsave("area_analyse.pdf", dpi = 600, height = 6, width = 15)
