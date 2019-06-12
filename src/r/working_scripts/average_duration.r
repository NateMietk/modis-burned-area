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

ci <- d %>%
  group_by(year, ecoregion) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, data=., repeated=F)),
         results = map(fit, confint))


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




# model it properly ------------------------------------------------------------
lmer(duration ~ year + (year|ecoregion), d) -> mod3
lm(duration ~ year*ecoregion, d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)

# ts <- mblm(duration ~ year, d, repeated = F)
ts <- mblm(duration_mean ~ year, dd, repeated = T)
ts_list <- list()
table_ <- tibble(ecoregion = NA, slope = NA, p = NA)
for(i in 1:length(unique(d$ecoregion))){
  print(i)
  ddd <- filter(dd, ecoregion == unique(d$ecoregion)[i])
  ts_list[[i]] <- mblm(duration_mean ~ year, ddd, repeated =F)
  summary(ts_list[[i]]) -> ss
  
  table_[i,1] <- unique(d$ecoregion)[i]
  table_[i,2] <- round(ts_list[[i]]$coefficients[2] %>% as.numeric,3)
  table_[i,3] <- round(ss$coefficients[2,4],5)
}
write_csv(table_, "images/ts_table.csv")

pp <- ggplot(dd, aes(x=year, y=duration_mean))+
  geom_point() +
  geom_line(aes(y=.fitted))+
  geom_line(aes(y=.fitted + .se.fit), lty=2)+
  geom_line(aes(y=.fitted - .se.fit), lty=2)+
  geom_line(aes(y = mod3, color = ecoregion))+
  facet_wrap(~ecoregion)+
  theme(legend.position = "none")

cowplot::ggdraw()+
  cowplot::draw_plot(pp,0,0,1,1)+
  cowplot::draw_grob(tableGrob(table_),.6,.05,.3,.2) +
  ggsave("images/ts_lmm.png", width=9, height = 7)

ggplot(d, aes(x = year, y=duration, color = ecoregion)) +
  #geom_point() +
  #geom_smooth()
  geom_line(aes(y=predict(mod2)))

#use theil-sen to look at significance of trend lines