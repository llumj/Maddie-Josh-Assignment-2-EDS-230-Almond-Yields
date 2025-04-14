
daily_almond_yield <- function(climate_data, time_series) {
  
  daily_data <- climate_data %>%
    group_by(day) %>% 
    filter(day == time_series & month == 2) %>% 
    ungroup()
  
  daily_summary <- daily_data %>%
    summarize(
      tmin_mean = mean(tmin_c, na.rm = TRUE),
      tmin_min = min(tmin_c, na.rm = TRUE),
      tmin_max = max(tmin_c, na.rm = TRUE),
      precip_mean= mean(precip, na.rm = TRUE),
      precip_min= min(precip, na.rm = TRUE),
      precip_max= max(precip, na.rm = TRUE)
    )
  
  almond_yield_min <- (-0.015*daily_summary$tmin_min) - (0.0046*(daily_summary$tmin_min^2))- (0.07*daily_summary$precip_min)+(0.0043*(daily_summary$precip_min^2))+0.28
  almond_yield_max <- (-0.015*daily_summary$tmin_max) - (0.0046*(daily_summary$tmin_max^2))- (0.07*daily_summary$precip_max)+(0.0043*(daily_summary$precip_max^2))+0.28
  almond_yield_mean <- (-0.015*daily_summary$tmin_mean) - (0.0046*(daily_summary$tmin_mean^2))- (0.07*daily_summary$precip_mean)+(0.0043*(daily_summary$precip_mean^2))+0.28
  
  almond_yield_predictions <- c(almond_yield_max, almond_yield_min, almond_yield_mean)
  return(almond_yield_predictions)
}
