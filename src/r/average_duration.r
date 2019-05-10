library(tidyverse)
library(lme4)
library(lmerTest)
library(mblm)
library(lubridate)
library(broom)

# read in daily data -----------------------------------------------------------
d <- read_csv("data/daily_asof_20180320.csv") %>% 
  mutate(duration = duration+1,
         year = as.numeric(substr(date,1,4))) %>%
  group_by(modis_id) %>%
  summarise(duration = mean(duration),
            year = mean(year),
            ecoregion = first(l1_ecoregion)) %>%
  ungroup()

peak_season <- read_csv("data/daily_asof_20180320.csv") %>%
  mutate(day = yday(date),
         year = as.numeric(substr(date,1,4))) %>%
  group_by(year, l1_ecoregion) %>%
  summarise(peak_day = mean(day),
            sd_peak = sd(day)) %>%
  ungroup()

# get yearly ecoregion climate variables and plot duration vs vpd, tmin, aet -----

dd <- d %>%
  group_by(year, ecoregion) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, data=., repeated=F)),
         results = map(fit, augment)) %>%
  unnest(results)


write_csv(dd,"data/yearly_duration_eco.csv")

# plot means ----------
ggplot(d, aes(x = as.factor(year), y=duration)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ecoregion)+
  ylim(low=0, high = 40)

ggplot(peak_season, aes(x = year, y=peak_day)) +
  geom_errorbar(aes(ymin = peak_day - sd_peak, ymax = peak_day+sd_peak))+
  facet_wrap(~l1_ecoregion)

ggplot(peak_season, aes(x=year, y=sd_peak))+
  geom_point() +
  facet_wrap(~l1_ecoregion)

ggplot(dd, aes(x=year, y=duration_mean))+
  geom_point() +
  geom_line(aes(y=.fitted))+
  geom_line(aes(y=.fitted + .se.fit), lty=2)+
  geom_line(aes(y=.fitted - .se.fit), lty=2)+
  geom_line(aes(y = mod3, color = ecoregion))+
  facet_wrap(~ecoregion)+
  theme(legend.position = "none")


# model it properly ------------------------------------------------------------
lmer(duration ~ year + (year|ecoregion), d) -> mod3
lm(duration ~ year*ecoregion, d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)

ts <- mblm(duration ~ year, d, repeated = F)
ts <- mblm(duration_mean ~ year, dd, repeated = F)
ts_list <- list()
for(i in unique(d$ecoregion)){
  print(i)
  ddd <- filter(dd, ecoregion == i)
  ts_list[[i]] <- mblm(duration_mean ~ year, dd, repeated = F)
  print(summary(ts_list[[i]]))
}

ggplot(d, aes(x = year, y=duration, color = ecoregion)) +
  #geom_point() +
  #geom_smooth()
  geom_line(aes(y=predict(mod2)))

#use theil-sen to look at significance of trend lines