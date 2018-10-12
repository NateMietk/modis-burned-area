

modelFSR <- fsr_vs %>%
  na.omit() %>%
  group_by(na_l1name) %>%
  do(fitFSR = lme4::lmer(fsr ~ area_km2 + duration, data = .))
modelFSR_coef <- broom::tidy(modelFSR, fitFSR, conf.int = TRUE)
modelFSR_pred <- broom::augment(modelFSR, fitFSR)
dfFSR_pred <- broom::glance(modelFSR, fitFSR)

modelFSR_coef %>%
  ggplot(aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))

dfFSR_pred %>%
  ggplot(aes(x = reorder(na_l1name, -r.squared), y = r.squared)) +
  geom_bar(stat = 'identity') +
  xlab('') + ylab('R Sqaure') +
  theme_pub() 

modelFSR_pred %>%
  ggplot(aes(x = fsr, y = .fitted)) +
  geom_point() +
  geom_smooth()


modelFSR <- fsr_vs %>%
  na.omit() %>%
  do(fitFSR = lme4::glmer(fsr ~ area_km2 + duration + (1|na_l1name), data = .))
modelFSR_coef <- broom::tidy(modelFSR, fitFSR, conf.int = TRUE)
modelFSR_pred <- broom::augment(modelFSR, fitFSR)
dfFSR_pred <- broom::glance(modelFSR, fitFSR)
