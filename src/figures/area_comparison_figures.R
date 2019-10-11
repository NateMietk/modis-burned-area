# comparing areas between mtbs and modis

library(tidyverse)
library(ggpubr)
library(scales)
library(strucchange)

#read_csv("tables/mtbs_modis_ids_ba_cast_s5t11.csv") -> d
d<- read_csv('data/cm_2019.csv') %>%
  arrange(mtbs_hectares)
output_table <- data.frame(max_area = NA,
                           r2 = NA,
                           n=NA,
                           slope = NA)
nn <- 50

for(i in 1:nn){
  e <- max(c(d$mtbs_hectares, d$modis_ha), na.rm = T)
  f <- (e/nn) * i
  g <- d[d$mtbs_hectares < f & d$mtbs_acres > f-(e/nn),]
  summary(lm(mtbs_hectares ~ modis_ha, g)) -> m
  output_table[i,1] <- f
  output_table[i,2] <- m$r.squared
  output_table[i,3] <- nrow(g)
  output_table[i,4] <- m$coefficients[2,1]
}

output_table$std_max_area <- output_table$max_area/e



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

breakpoints(output_table$r2~output_table$max_area)
rrr<-paste("R^2 == ",round(m$r.squared,2))
p1 <- ggplot(d, aes(modis_ha, mtbs_hectares))+
  geom_smooth(method = "lm") +
  geom_point(alpha = 0.15) +
  geom_abline(slope=1, intercept=0) +
  theme_pubr(base_size = 14) +
  theme(axis.text = element_text(size = 10)) +
  coord_fixed() +
  xlab("MODIS (hectares)") +
  ylab("MTBS (hectares)") +
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+
  annotate("text",  x=153000, y=130000, label = "1:1 line") +
  annotate("text",  x=100000, y=150000, col="blue", 
           label = rrr, parse=T) +
  theme(axis.title = element_text(face="bold"))


p2 <- ggplot(output_table, aes(max_area, r2)) +
  geom_hline(yintercept = .80, col="grey20", lty=2)+
  geom_line() +
  #geom_line(aes(y= pred), col="red")+
 # coord_fixed()+
  theme_pubr(base_size = 14) +
  #ylab(expression(R^2))+
  ylab("Variance Explained") +
  theme(axis.text = element_text(size = 10)) +
  xlab("MTBS Area (hectares)")+
  scale_x_continuous(labels = comma) +
  theme(axis.title = element_text(face="bold"))#+
  # annotate("text",  x=100000, y=0.4, label = paste("Linear model for every", 
  #                                                   round(max(d$mtbs_hectares)/nn), 
  #                                                 "hectare segement.",
  #                                                 "Average n = ", mean(output_table$n)))

p3 <- ggplot(output_table, aes(max_area, slope)) +
  geom_hline(yintercept = 1, col="grey20", lty=2)+
  geom_line() +
  theme_pubr(base_size = 14) +
  theme(axis.text = element_text(size = 10))+
  xlab("MTBS Area (hectares)")+
  scale_x_continuous(labels = comma)+
  ylab("Estimate")+
  theme(axis.title = element_text(face="bold"))

ggarrange(p1, p2, p3, nrow = 1, ncol=3, labels = "AUTO") +
  ggsave("images/area_analyse.pdf", dpi = 600, height = 4.5, width = 13)
ggarrange(p1, p2,p3, nrow = 1, ncol=3, labels = "AUTO") +
  ggsave("images/area_analyse.png", dpi = 400, height = 4.5, width = 13,
        limitsize = T)

