libs <- c("tidyverse", "lme4", "lmerTest", "mblm", "lubridate",
          "broom", "nlme","ggpubr")
lapply(libs, library, character.only = TRUE)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# read in daily data -----------------------------------------------------------
d <- read_csv("data/daily_stats_02-18_asof_20180612.csv") %>% 
  mutate(duration = duration,
         year = as.numeric(substr(date,1,4))) %>%
  group_by(id) %>%
  summarise(duration = getmode(duration),
            year = getmode(year),
            ecoregion = getmode(l1_ecoregion),
            lc = getmode(lc_name)) %>%
  ungroup()

dd <- d %>%
  dplyr::select(year, ecoregion, duration) %>%
  group_by(year, ecoregion) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, 
                                data=., repeated=F)),
         results = map(fit, augment)) %>%
  unnest(results)

dd_lme <- d %>%
  dplyr::select(year, ecoregion, duration) %>%
  nest(-ecoregion) %>%
  mutate(fit = map(data, ~ lm(duration ~ year,
                               data = .)),
         results = map(fit, augment)) %>%
  unnest(results)

write_csv(dd,"data/yearly_duration_eco.csv")

dd_lc <- d %>%
  dplyr::select(year, lc, duration) %>%
  group_by(year, lc) %>%
  summarise(duration_mean = mean(duration),
            duration_sd = sd(duration)) %>%
  ungroup()%>%
  nest(-lc) %>%
  mutate(fit = map(data, ~ mblm(duration_mean ~ year, data=., repeated=F)),
         results = map(fit, augment)) %>%
  unnest(results)

dd_lc_lme <- d %>%
  dplyr::select(year, lc, duration) %>%
  nest(-lc) %>%
  mutate(fit = map(data, ~ lm(duration ~ year,
                              data = .)),
         results = map(fit, augment)) %>%
  unnest(results)

write_csv(dd_lc, "data/yearly_duration_lc.csv")

# plot means ----------
ggplot(d, aes(x = as.factor(year), y=duration)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~lc)+
  ylim(low=0, high = 40)
ggplot(d, aes(x = as.factor(year), y=duration)) +
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~ecoregion)+
  ylim(low=0, high = 40)


# model it properly - ecoregion ------------------------------------------------
dd$mod3 <- dd_lme$.fitted

lmer(duration ~ year + (year|ecoregion), d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)

lm(duration ~ year*ecoregion, d) -> mod3
dd$mod3 = predict(mod3, newdata = dd)


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
  geom_line(aes(y=.fitted + (.se.fit*1.96)), lty=2)+
  geom_line(aes(y=.fitted - (.se.fit*1.96)), lty=2)+
  geom_line(data = dd_lme, aes(y = .fitted, color = ecoregion))+
  geom_line(data = dd_lme, aes(y = .fitted + (.se.fit*1.96), color = ecoregion), lty=3)+
  geom_line(data = dd_lme, aes(y = .fitted - (.se.fit*1.96), color = ecoregion), lty=3)+
  facet_wrap(~ecoregion, scales = "free")+
  theme_pubr() +
  theme(legend.position = "none");pp

cowplot::ggdraw()+
  cowplot::draw_plot(pp,0,0,1,1)+
  cowplot::draw_grob(tableGrob(table_, rows = NULL,
                               theme = ttheme_minimal(base_size =10)),
                     .6,.07,.2,.2) +
  ggsave("images/ts_lmm.png", width=9, height = 7)

ggplot(d, aes(x = year, y=duration, color = ecoregion)) +
  #geom_point() +
  #geom_smooth()
  geom_line(aes(y=predict(mod2)))

# by landcover --------------------------------------------------------
lmer(duration ~ year + (year|lc), d) -> mod3
dd_lc$mod3 = predict(mod3, newdata = dd_lc)

lm(duration ~ year*lc, d) -> mod3
dd_lc$mod3 = predict(mod3, newdata = dd_lc)


ts_list <- list()
table_ <- tibble(lc = NA, slope = NA, p = NA)
for(i in 1:length(unique(d$lc))){
  print(i)
  ddd <- filter(dd_lc, lc == unique(d$lc)[i])
  ts_list[[i]] <- mblm(duration_mean ~ year, ddd, repeated =F)
  summary(ts_list[[i]]) -> ss
  
  table_[i,1] <- unique(d$lc)[i]
  table_[i,2] <- round(ts_list[[i]]$coefficients[2] %>% as.numeric,3)
  table_[i,3] <- round(ss$coefficients[2,4],5)
}
write_csv(table_, "images/ts_lc_table.csv")

pp <- ggplot(dd_lc, aes(x=year, y=duration_mean))+
  geom_point() +
  geom_line(aes(y=.fitted))+
  geom_line(aes(y=.fitted + (.se.fit*1.96)), lty=2)+
  geom_line(aes(y=.fitted - (.se.fit*1.96)), lty=2)+
  geom_line(data = dd_lc_lme, aes(y = .fitted, color = lc))+
  geom_line(data = dd_lc_lme, aes(y = .fitted + (.se.fit*1.96), color = lc), lty=3)+
  geom_line(data = dd_lc_lme, aes(y = .fitted - (.se.fit*1.96), color = lc), lty=3)+
  facet_wrap(~lc, scales = "free")+
  theme_pubr() +
  theme(legend.position = "none")+
  ggsave("images/ts_lm_lc.png", width=10, height = 10);pp

cowplot::ggdraw()+
  cowplot::draw_grob(tableGrob(table_, rows = NULL,
                               theme = ttheme_minimal())) +
  ggsave("images/ts_lmm_table.png")
# gmod<-gls(duration ~ year, d[d$lc == "Barren",], correlation = corAR1(form = ~1|year))
# 
# ggplot(d[d$lc == "Barren",], aes(x = year, y=duration)) +
#   geom_line(aes(y=predict(gmod)))
