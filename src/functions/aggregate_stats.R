aggregate_stats <- function(df, g_var, var) {
  var_group <- ensym(g_var)
  var_prime <- ensym(var)
  
  var1 <- paste0('sum_', var_prime)
  var2 <- paste0('min_', var_prime)
  var3 <- paste0('max_', var_prime)
  var4 <- paste0('mean_', var_prime)
  var5 <- paste0('median_', var_prime)
  var6 <- paste0('sd_', var_prime)
  var7 <- paste0('se_', var_prime)
  var8 <- paste0('lower_95ci_', var_prime)
  var9 <- paste0('upper_95ci_', var_prime)
  
  
  df1 <- df %>%
    group_by(!!var_group) %>%
    summarise(n_fire_events = n(),
              !!ensym(var1) := sum(!!var_prime, na.rm = TRUE),
              !!ensym(var2) := min(!!var_prime, na.rm = TRUE),
              !!ensym(var3) := max(!!var_prime, na.rm = TRUE),
              !!ensym(var4) := mean(!!var_prime, na.rm = TRUE),
              !!ensym(var5) := median(!!var_prime, na.rm = TRUE),
              !!ensym(var6) := mean(!!var_prime, na.rm = TRUE))  %>%
    mutate(!!ensym(var7) := !!ensym(var6) / sqrt(n_fire_events),
           !!ensym(var8) := !!ensym(var4) - qt(1 - (0.05 / 2), n_fire_events - 1) * !!ensym(var7),
           !!ensym(var9) := !!ensym(var4) + qt(1 - (0.05 / 2), n_fire_events - 1) * !!ensym(var7))
  return(df1)
}
