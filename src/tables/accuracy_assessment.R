big_tables <- list.files(file.path(version_dir, 'final_tables_casted'), full.names = TRUE)
big_tables <- lapply(big_tables, function(x) {
  tbl <- read_csv(x)
  return(tbl)
})

assess_accuracy_df <- do.call('rbind', big_tables) %>%
  mutate(st_combo = as.factor(st_combo),
         abs_totals = (modisT_mtbsF_modis_over_th + modisT_mtbsT + modisF_mtbsT),
         # total mtbs events
         total_mtbs = modisT_mtbsT + modisF_mtbsT,
         # total modis events
         total_modis = modisT_mtbsF_modis_over_th + modisT_mtbsT, 
         # what is the proportion of modis to mtbs fire, regadless of multiple ids
         modis_to_mtbs_all = modisT_mtbsF_modis_over_th/modisF_mtbsT, 
         # what is the proportion of modis to mtbs fire, where there are only 1 modis ids
         modis_to_mtbs_id_1 = mtbsT_modisT_unique_modis_events/modisF_mtbsT,
         # Overall, how often is the classifier correct?
         accuracy = modisT_mtbsT / abs_totals,
         # Overall, how often is it wrong?
         misclassification_rate = (modisT_mtbsF_modis_over_th + modisF_mtbsT)/ abs_totals,
         # When it's actually yes, how often does it predict yes?
         true_positive_rate = modisT_mtbsT / (modisF_mtbsT	+ modisT_mtbsT),
         # When it predicts yes, how often is it correct?
         precision = modisT_mtbsT / total_modis,
         # How often does the yes condition actually occur in our sample?
         prevelance = (modisF_mtbsT + modisT_mtbsT) / abs_totals) 

assess_accuracy <- assess_accuracy_df %>%
  dplyr::select(-mtbs_IDs_of_max_modis,
                -row_check,
                -which_max_mtbs_per_modis,
                -which_max_modis_per_mtbs,-X1) %>%
  gather(key, value, -st_combo) %>%
  filter(key == "accuracy" |
           key == "misclassification_rate"|
           key == "true_positive_rate"|
           key == "precision"|
           key == "prevelance") %>%
  spread(key, value)
write.csv(assess_accuracy, "results/draft_tables/performance_metrics.csv")

confusion_matrix <- assess_accuracy_df %>%
  filter(st_combo == 's5t11')

confusion_matrix <- data.frame(
  MTBS_True = c(confusion_matrix$modisT_mtbsT[1], confusion_matrix$modisF_mtbsT[1]),
  MTBS_False_over1000 = c(confusion_matrix$modisT_mtbsF_modis_over_th,""),
  MTBS_False_under1000 = c(confusion_matrix$modisT_mtbsF_all_modis - confusion_matrix$modisT_mtbsF_modis_over_th,""),
  row.names = c("MCD64 True", "MCD64 False"))
