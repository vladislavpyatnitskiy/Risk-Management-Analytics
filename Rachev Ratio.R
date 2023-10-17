# Function to calculate Rachev Ratio via historical method
Rachev.ratio <- function(x, y, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Error. Insufficient number of observations.") } 
  
  # Otherwise calculate logs & remove NA if necessary & create list for CVaR
  else { if (isTRUE(lg)) { x <- diff(log(x))[-1,] } 
    
    # Create variable to contain values for positive and negative CVaRs
    list_for_cvar_minus <- NULL
    list_for_cvar_max <- NULL
    
    # For each column in dataset assign variable to each column
    for (n in 1:ncol(x)){ security <- x[,n]
    
      # Sort values ascendingly
      testcvar <- security[order(security),]
      
      # Select 5% worst observations, find their means and add to list
      list_for_cvar_minus <- rbind(list_for_cvar_minus,
                                   mean(testcvar[1:((1 - y * 0.01) *
                                                      nrow(testcvar)),])) 
      
      # Select 5% best observations, find their means and add to list
      list_for_cvar_max <- rbind(list_for_cvar_max,
                                 mean(testcvar[((y * 0.01) *nrow(testcvar)):
                                                 nrow(testcvar),]))
    }
    # Calculate Rachev Ratio
    rr.coef <- round(list_for_cvar_max / (-list_for_cvar_minus), 3)
    
    # Put name for column
    colnames(rr.coef) <- sprintf("Rachev Ratio at %s%%", y)
    
    # Give them names from data set
    rownames(rr.coef) <- colnames(x)
    
    # Display matrix
    return(rr.coef) }
}
# Test
Rachev.ratio(stock_data, 95)
