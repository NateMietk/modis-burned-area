library(tidyverse)
library(lme4)
library(lmerTest)

d <- read_csv("data/daily_asof_20180320.csv") %>% 
  mutate(duration = duration+1,
         year = as.numeric(substr(date,1,4))) 

# get yearly ecoregion climate variables and plot duration vs vpd, tmin, aet

dd <- d %>%
  group_by(year, l1_ecoregion) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration))

write_csv(dd,"data/yearly_duration_eco.csv")

ggplot(dd, aes(x = year, y=duration_mean, color = l1_ecoregion)) +
  geom_line() +
  geom_smooth(method = "lm")

lmer(duration ~ year + (year|l1_ecoregion), d) -> mod2
lm(duration ~ year*l1_ecoregion, d) %>% summary

ggplot(d, aes(x = year, y=duration, color = l1_ecoregion)) +
  #geom_point() +
  #geom_smooth()
  geom_line(aes(y=predict(mod2)))

#use theil-sen to look at significance of trend lines