library(tidyverse)

irbs<-read_csv("tables/intrayear_reburns.csv") %>%
  select(-X1) %>%
  mutate(tile = substr(observation,10,15),
         year = substr(observation,16,20)) %>%
  group_by(tile) %>%
  summarise(mean_reburn_pct=mean(reburn_pct),sd_reburn_pct = sd(reburn_pct))

write_csv(irbs, "tables/reburn_stats.csv")
