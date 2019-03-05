ecoreg1_fsr_df <-   as.data.frame(lvl1_eco_fsr_ts) %>%
  dplyr::select(na_l1name, year, mean_fsr) %>%
  na.omit() %>%
  mutate(log_mean_fsr = log(mean_fsr),
         year = as.integer(year),
         cyear = c(scale(year))) %>%
  group_by(na_l1name) %>%
  nest()

# Linear Model
# build nested tibble
fund_nested <- ecoreg1_fsr_df %>%
  mutate(lm_mod = map(data, 
                      ~lm(formula = mean_fsr ~ cyear, 
                          data = .x))) 

fund_models_aug <-
  fund_nested %>%
  mutate(aug = map(lm_mod, ~augment(.x))) %>% 
  unnest(aug)

pct_increase <- fund_models_aug %>%
  group_by(na_l1name) %>%
  summarise(pct_inc = (last(.fitted) - first(.fitted))*100) %>%
  mutate(x_val = -0.5,
         y_val = 12.75)

sp <- ggscatter(fund_models_aug, x = "cyear", y = "mean_fsr",
                color = "na_l1name", palette = "jco",
                add = "reg.line", conf.int = TRUE) +
  stat_cor(aes(color = na_l1name, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -1.25,  label.y = 14) +
  geom_text(data = pct_increase, aes(color = na_l1name, x = round(x_val, 3), y = round(y_val, 3), label = paste('%Inc = ', round(pct_inc, 3)))) +
  facet_wrap(~na_l1name) +
  theme(legend.position = 'none')
ggsave(file = 'results/yearly_mean_fsr_ecoreg1.pdf', sp, width = 15, height = 9, dpi=1200, scale = 2, units = "cm")


# Theil-Sen model
# build nested tibble
fund_nested <- ecoreg1_fsr_df %>%
  mutate(mblm_mod = map(data, 
                      ~mblm::mblm(formula = mean_fsr ~ cyear, 
                          data = .x))) 

fund_models_aug <-
  fund_nested %>%
  mutate(aug = map(mblm_mod, ~augment(.x))) %>% 
  unnest(aug)

pct_increase <- fund_models_aug %>%
  group_by(na_l1name) %>%
  summarise(pct_inc = (last(.fitted) - first(.fitted))*100) %>%
  mutate(x_val = -0.5,
         y_val = 12.75)

sp_mblm <- ggscatter(fund_models_aug, x = "cyear", y = "mean_fsr",
                color = "na_l1name", palette = "jco",
                add = "reg.line", conf.int = TRUE) +
  stat_cor(aes(color = na_l1name, label = paste(..rr.label.., ..p.label.., sep = "~`,`~")), label.x = -1.25,  label.y = 14) +
  geom_text(data = pct_increase, aes(color = na_l1name, x = round(x_val, 3), y = round(y_val, 3), label = paste('%Inc = ', round(pct_inc, 3)))) +
  facet_wrap(~na_l1name) +
  theme(legend.position = 'none')
ggsave(file = 'results/yearly_mean_fsr_ecoreg1_mblm.pdf', sp_mblm, width = 15, height = 9, dpi=1200, scale = 2, units = "cm")

