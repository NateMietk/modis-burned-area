
##### the fields in st_config

# modisT_mtbsT: number of mtbs fires with at least one modis fire pixel
# modisF_mtbsT: number of mtbs fires with 0 modis pixels
# modisT_mtbsF_all_modis: number of total modis IDs MINUS the modisT_mtbsT column   --- first part calculated from the modis rasters only
# modisT_mtbsF_modis_over_th: number of total modis IDS over the threshold MINUS modisT_mtbsT column ---  first part calculated from modis rasters only
# mtbs_w_multiple_modis: mtbs fires containing multiple modis IDs
# st_combo: space time combo
# total_mtbs: first two columns added
# total_modis_over_th: modisT_mtbsT + modisT_mtbsF_modis_over_th
# diff: absolute value of total mtbs -  total_modis

# description of the accuracy assessment varaibles can be found here: http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/

library(plot3D)
library(dplyr)
library(scales)
# library(stargazer)
library(knitr)
version_dir <- "/home/a/Downloads"

st_config <- read_csv(file.path(version_dir, 'big_table_s5t11.csv')) %>%
  #left_join(., read_csv(file.path(version_dir, 'confusion_matrix', 'additional_variables_confusion_matrix.csv')), by = 'st_combo') %>%
  separate(st_combo, c('space', 'time'), sep = 3) %>% 
  mutate(
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
    normalized = rescale(mtbs_w_multiple_modis, to = c(1, 0)),
    scaled_accuracy = accuracy*normalized,
    # Overall, how often is it wrong?
    misclassification_rate = (modisT_mtbsF_modis_over_th + modisF_mtbsT)/ abs_totals,
    # When it's actually yes, how often does it predict yes?
    true_positive_rate = modisT_mtbsT / (modisF_mtbsT	+ modisT_mtbsT),
    # When it predicts yes, how often is it correct?
    precision = modisT_mtbsT / total_modis,
    # How often does the yes condition actually occur in our sample?
    prevelance = (modisF_mtbsT + modisT_mtbsT) / abs_totals,
    ## Calculating the KAPPA
    # observed proportionate agreement
    # p_0 = modisT_mtbsT/abs_totals,
    # p_yes = ((modisT_mtbsT + modisF_mtbsT)/abs_totals) * (modisT_mtbsT + modisT_mtbsF_modis_over_th)/abs_totals,
    # p_no = (modisT_mtbsF_modis_over_th/abs_totals) * (modisF_mtbsT/abs_totals),
    # p_e = p_yes + p_no,
    # kappa = (p_0 - p_e)/(1-p_e),
    # cleaning for plotting
    space = as.numeric(gsub('s|t', '', space)),
    time = as.numeric(gsub('s|t', '', time))) 

st_long <- st_config %>%
  dplyr::select(-mtbs_IDs_of_max_modis,
                -row_check,
                -which_max_mtbs_per_modis,
                -which_max_modis_per_mtbs,
                -X1
  ) %>%
  gather() %>%
  filter(key == "accuracy" |
           key == "normalized" |
           key == "scaled_accuracy"|
           key == "misclassification_rate"|
           key == "true_positive_rate"|
           key == "precision"|
           key == "prevelance")


dir.create("tables")
write.csv(st_long, "tables/performance_metrics.csv")

kable(st_long, digits = 2, caption = "Performance metrics")

confusion_matrix <- data.frame(
  MTBS_True = c(st_config$modisT_mtbsT[1], st_config$modisF_mtbsT[1]),
  MTBS_False = c(paste(st_config$modisT_mtbsF_modis_over_th, "(over threshold)"),""),
  MTBS_False = c(paste(st_config$modisT_mtbsF_all_modis - st_config$modisT_mtbsF_modis_over_th, "(under threshold)"),""),
  row.names = c("MCD64 True", "MCD64 False"))

write.csv(confusion_matrix, "tables/confusion_matrix.csv")
