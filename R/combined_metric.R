#' lowflowmetrics
#'
#' Compute the Erica-Colleen metric for range of May flows
#' @param  mod  model estimates
#' @param  obs  observations
#' @param year years of observed and modeled data (from dataframe)
#' @param month months of observed and modeled data (from dataframe)
#' @param  flow_year year of interest (default is our calibration year, 1974)
#' @param flow_months which to use default (May is default)
#' @return ecm_metric, the square root of the normalized sum of squared errors and the pearson correlation coefficient 

ecm <- function(mod, obs, year, month, flow_months=5, flow_year=1974){
  
  #create combined dataframe
  flow <- cbind.data.frame(mod, obs, year, month)
  
  #get min and max for year of interest
  tmp = flow %>% 
    filter(year == flow_year) %>% 
    group_by(month) %>% 
    dplyr::summarize(mino=min(obs), 
                     maxo=max(obs),
                     minm=min(mod),
                     maxm=max(mod)) %>% 
    filter(month == flow_months)
  #get ranges
  range_o <- tmp$maxo - tmp$mino
  range_m <- tmp$maxm - tmp$minm
  
  #calculate range difference
  range_diff <- 1 - abs(range_o - range_m)
  
  #sse 
    sse <- sum(mean((obs - mod)^2))
    sse_norm <- 1 - sse
  
  #combined metric
  ecm_metric <- sqrt(abs(range_diff*sse_norm))
  
  return(ecm_metric)
}