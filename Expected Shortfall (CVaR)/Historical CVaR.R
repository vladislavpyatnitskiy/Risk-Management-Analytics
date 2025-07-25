histCVaR <- function(x, CVaR, lg=T){ # Expected Shortfall via historical method
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) return(message("Insufficient number of observations."))
  
  if (lg) x = diff(log(x))[-1,] # log if necessary
    
  l <- NULL # Create variable to contain values for CVaR
    
  for (n in 1:ncol(x)){ s <- x[,n] # Assign variable to each column
    
    v <- s[order(s),] # Sort values in ascending way
    
    # Select 5% worst observations, find their means and add to list
    l <- rbind(l, mean(v[1:((1 - CVaR * 0.01) * nrow(v)),])) }
    
  colnames(l) <- sprintf("CVaR %s%%", CVaR) # Put name for column
    
  rownames(l) <- colnames(x) # Give them names from data set
    
  return(l) # Display matrix
}
histCVaR(stock_data, 95, T) # Test
