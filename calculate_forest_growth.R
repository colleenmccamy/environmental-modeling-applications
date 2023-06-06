### model to calculate forest growth

calculate_forest_growth <- function(time, C, params){
  
  if (hr = NULL){
  
  # providing equation if carbon is greater than threshold canopy closure
  if (C < params$thresh){
    
    carbon_change = params$r * C - (params$hr *C)
    
  }
  
  # providing the equation if it less than threshold canopy closure
  else {
    
    carbon_change = params$g * (1 - C/params$k) - (params$hr *C)
    
  } }
  
  else{ 
    
    if (C < params$thresh){
      
      carbon_change = params$r * C - (params$hr * C)
      
    }
    
    # providing the equation if it less than threshold canopy closure
    else {
      
      carbon_change = params$g * (1 - C/params$k) - (params$hr * C)
      
    } 
    
    
    }
  
  return(list(carbon_change))
  
}