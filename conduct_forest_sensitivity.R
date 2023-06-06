conduct_forest_sensitivity <- function(r, k, g, thresh, 
                                C_initial, time, func, hr = NULL){
  
  params = list(r = r, 
                k = k, 
                g = g, 
                thresh = thresh, 
                hr = hr)
  
  results = ode(y = C_initial, time = time, 
               func = func, parms = params)
  colnames(results) <- c("time", "forest_growth")
  
  all_results <- as.data.frame(results)
  max_growth <- max(all_results$forest_growth)
  
  return(max_growth)
  
}