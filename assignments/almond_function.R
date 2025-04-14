
daily_almond_yield <- function(climate_data, Tn1 = -0.015, Tn2 = -0.0046, P1 = -0.07, P2 = 0.0043, i = 0.28) {
  
  min_feb_temp_c <- climate_data %>%
    filter(month == 2) %>% 
    group_by(year) %>%
    summarize(min_feb_temp_c = min(tmin_c, na.rm = TRUE)) %>% 
    ungroup()
  
  jan_precip <- climate_data %>%
    filter(month == 1) %>% 
    group_by(year) %>% 
    summarize(jan_p = sum(precip, na.rm = TRUE)) %>% 
    ungroup()
  
  climate_data_jan_feb <- left_join(feb_min_t, jan_precip, by = "year")
  
  # Compute yield for each year
  climate_data <- climate_data %>%
    mutate(yield = Tn1 * min_feb_temp_c + Tn2 * min_feb_temp_c^2 + P1 * jan_precip + P2 * jan_precip^2 + i)
  
  
  
  # Return max, min, and mean yields
  return(list(
    maxyield = max(climate_data$yield, na.rm = TRUE),
    minyield = min(climate_data$yield, na.rm = TRUE),
    meanyield = mean(climate_data$yield, na.rm = TRUE)
  ))
}

