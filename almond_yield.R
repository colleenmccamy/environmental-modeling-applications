### -----------------------
# Function Documentation
### -----------------------

#' Calculating Almond Yield Based on Climate Data
#' 
#' Please load the tidyverse library to use this function 
#' 
#' 
#' @param years The years of interest with default years being 1989 to 2003. Please input years as a vector ex. c(1989, 1999, 2000).Note: years do not need to be consecutive.
#' @param climate_data This input takes the data as a dataframe with year, days, month, tmin_c (daily minimum temperature in degrees Celsius), and precip (the daily precipitation in millimeters.)
#'
#' @return A dataframe with minimum, maximum and average almond yield anomaly in ton/acres.
#' @export
#'
#' 
#' 


almond_yield <- function(years = c(1989:2003), climate_data){
  
  ### -----------------------
  # cleaning the data
  ### -----------------------
  
  # filtering based on input years
  climate_data <- climate_data |> 
    filter(year %in% years)
  
  # calculating the minimum temp values for Feb,
  minT2 <- climate_data |> 
    filter(month == 2) |> 
    group_by(year) |> 
    summarize(min_tmin_c = min(tmin_c))
  
  # calculating the total precipitation values for Jan,
  P1 <- climate_data |> 
    filter(month == 1) |> 
    group_by(year) |> 
    summarize(total_precip_mm = sum(precip))
  
  # creating a table to pull formula inputs from
  input_table <- full_join(minT2, P1, by = "year")
  
  ### -----------------------
  # calculating almond yields
  ### -----------------------
  
  
  # establishing an empty dataframe to populate
  yield_df <- data.frame(year = numeric(length(years)), yield = numeric(length(years)))
  
  # looping through each year to calculate the yield for the year
  for (i in seq_along(years)){
    
    # formula inputs
    year <- years[i]
    minT2 <- input_table$min_tmin_c[i]
    P1 <- input_table$total_precip_mm[i]
    
    # formula to calculate yield
    yield <- ((-0.015* minT2) - (0.0046 * (minT2)^2) - (.07 * P1) + (0.0043 * (P1)^2) + 0.28)
    
    # populating the dataframe with yield and year values
    yield_df[i, ] <- c(year, yield)
  }
  
  # calculating the mean, min and max anamolies for given years
  min_anomaly <- round(min(yield_df$yield), 3)
  max_anomaly <- round(max(yield_df$yield), 3)
  avg_anomaly <- round(mean(yield_df$yield), 3)
  
  anomaly_df <- tibble(anomaly = c("min_anomaly", "max_anomaly", "avg_anomaly"),
                       value = as.numeric(c(min_anomaly, max_anomaly, avg_anomaly)))
  
  return(anomaly_df)
}