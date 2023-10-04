# Function to get Expected Shortfall via historical method
histCVaR <- function(x, y, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Error. Insufficient number of observations.") } 
  
  # Otherwise calculate logs & remove NA if necessary & create list for CVaR
  else { if (isTRUE(lg)) { x <- diff(log(x))[-1,] } 

    # Create variable to contain values for CVaR
    list_for_cvar <- NULL
    
    # For each column in dataset assign variable to each column
    for (n in 1:ncol(x)){ security <- x[,n]
      
      # Sort values ascendingly
      testcvar <- security[order(security),]
      
      # Select 5% worst observations, find their means and add to list
      list_for_cvar <- rbind(list_for_cvar,
                             mean(testcvar[1:((1 - y * 0.01) *
                                                nrow(testcvar)),])) }
    # Put name for column
    colnames(list_for_cvar) <- sprintf("CVaR %s%%", y)
    
    # Give them names from data set
    rownames(list_for_cvar) <- colnames(x)
    
    # Display matrix
    return(list_for_cvar) }
}
# Test
histCVaR(stock_data, 95)
