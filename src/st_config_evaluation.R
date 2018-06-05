
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

# description of the accuracy assessment http://www.dataschool.io/simple-guide-to-confusion-matrix-terminology/
library(plot3D)
library(scales)

st_config <- read_csv(file.path(version_dir, 'confusion_matrix', 'confusion_matrices.csv')) %>%
  left_join(., read_csv(file.path(version_dir, 'confusion_matrix', 'additional_variables_confusion_matrix.csv')), by = 'st_combo') %>%
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
    modis_to_mtbs_id_1 = num_modisIDs1_and_mtbsT/modisF_mtbsT,
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

scatter3D(st_config$space,#x
          st_config$time, #y
          st_config$modis_to_mtbs_all, #z
          ticktype = "detailed")

scatter3D(st_config$space,#x
          st_config$time, #y
          st_config$modis_to_mtbs_id_1, #z
          ticktype = "detailed")


scatter3D(st_config$space,#x
          st_config$time, #y
          st_config$scaled_accuracy, #z
          ticktype = "detailed")

# what is the optimal s-t configuration for all fires 
which(abs(st_config$modis_to_mtbs_id_1-1) == min(abs(st_config$modis_to_mtbs_id_1-1)))

# what is the optimal s-t configuration where there are only 1 modis ID within a MTBS perimeter
which(abs(st_config$modis_to_mtbs_all-1) == min(abs(st_config$modis_to_mtbs_all-1)))

  
  
