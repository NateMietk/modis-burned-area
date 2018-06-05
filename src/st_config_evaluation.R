


st_config <- read_csv(file.path(version_dir, 'confusion_matrix', 'confusion_matrices.csv')) %>%
  left_join(., read_csv(file.path(version_dir, 'confusion_matrix', 'additional_variables_confusion_matrix.csv')), by = 'st_combo') %>%
  separate(st_combo, c('space', 'time'), sep = 3) %>% 
  mutate(
    # total mtbs events
    total_mtbs = modisT_mtbsT + modisF_mtbsT,
    # total modis events
    total_modis = modisT_mtbsF_modis_over_th + modisT_mtbsT, 
    # what is the proportion of modis to mtbs fire, regadless of multiple ids
    modis_to_mtbs_all = modisT_mtbsF_modis_over_th/modisF_mtbsT, 
    # what is the proportion of modis to mtbs fire, where there are only 1 modis ids
    modis_to_mtbs_id_1 = num_modisIDs1_and_mtbsT/modisF_mtbsT,
    # cleaning for plotting
    space = as.numeric(gsub('s|t', '', space)),
    time = as.numeric(gsub('s|t', '', time))) 

library(plot3D)


scatter3D(st_config$space,#x
          st_config$time, #y
          st_config$modis_to_mtbs_all, #z
          ticktype = "detailed")

scatter3D(st_config$space,#x
          st_config$time, #y
          st_config$modis_to_mtbs_id_1, #z
          ticktype = "detailed")

# what is the optimal s-t configuration for all fires 
which(abs(st_config$modis_to_mtbs_id_1-1) == min(abs(st_config$modis_to_mtbs_id_1-1)))

# what is the optimal s-t configuration where there are only 1 modis ID within a MTBS perimeter
which(abs(st_config$modis_to_mtbs_all-1) == min(abs(st_config$modis_to_mtbs_all-1)))

  
  
