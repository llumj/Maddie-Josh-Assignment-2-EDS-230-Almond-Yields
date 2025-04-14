#' Calculate Almond Yield
#'
#' @description Computes the maximum, minimum, and mean almond yield for each year 
#' based on climate data inputs such as minimum February temperature and January precipitation.
#'
#' @author Madison Enda and Joshua Mull for EDS230 with Naomi Tague
#' 
#' @param climate_data A data frame containing climate data with columns for `month`, `year`, 
#' `tmin_c` (minimum daily temperature in Celsius), and `precip` (daily precipitation in millimeters).
#' @param Tn1 Coefficient for the minimum February temperature.
#' Default is -0.015.
#' @param Tn2 Quadratic term coefficient of the minimum February temperature.
#' Default is -0.0046.
#' @param P1 Coefficient for January precipitation. Default is -0.07.
#' @param P2 Quadratic term coefficient of January precipitation. Default is 0.0043.
#' @param i Intercept term 0.28.
#'
#' @return A list containing: min, max and mean almond yields across all years 

almond_yield <- function(climate_data, Tn1 = -0.015, Tn2 = -0.0046, P1 = -0.07, P2 = 0.0043, i = 0.28) {
  
  # filter data for min temp in February 
  feb_temp <- climate_data %>%
    filter(month == 2) %>% 
    group_by(year) %>%
    summarize(min_feb_temp_c = min(tmin_c, na.rm = TRUE), .groups = "drop") 
  
  # filter data for total rain in January 
  jan_p <- climate_data %>%
    filter(month == 1) %>% 
    group_by(year) %>% 
    summarize(jan_precip = sum(precip, na.rm = TRUE), .groups = "drop") 
  
  # join data frames 
  climate_data_jan_feb <- left_join(feb_temp, jan_p, by = "year")
  
  # Compute yield for each year
  climate_data_jan_feb <- climate_data_jan_feb %>%
    mutate(yield = Tn1 * min_feb_temp_c + Tn2 * min_feb_temp_c^2 + P1 * jan_precip + P2 * jan_precip^2 + i)
  
  
  
  # Return max, min, and mean yields
  return(list(
    maxyield = max(climate_data_jan_feb$yield, na.rm = TRUE),
    minyield = min(climate_data_jan_feb$yield, na.rm = TRUE),
    meanyield = mean(climate_data_jan_feb$yield, na.rm = TRUE)
  ))
}
