library(tidyverse)


# loading reburn stats calculated from other script, summarizing by tile
irbs<-read_csv("tables/intrayear_reburns.csv") %>%
  mutate(tile = substr(observation,10,15),
         year = substr(observation,16,20)) %>%
  group_by(tile) %>%
  summarise(mean_reburn_pct=mean(reburn_pct),sd_reburn_pct = sd(reburn_pct),
            mean_n2 = mean(n2_or_more), sum(n2_or_more))

write_csv(irbs, "tables/reburn_stats.csv")

# everything but florida (h10v06) since that has significantly more reburns
all_but_florida <- read_csv("tables/intrayear_reburns.csv") %>%
  mutate(tile = substr(observation,10,15)) %>%
  filter(tile != "h10v06") %>%
  summarise(mean_reburn_pct=mean(reburn_pct),sd_reburn_pct = sd(reburn_pct),
            mean_n2 = mean(n2_or_more), sum(n2_or_more))
