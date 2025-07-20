histVaR <- function(x, VaR, lg = T){ # Calculate Historical VaR
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) return(message("Insufficient number of observations."))
    
  if (lg) x = diff(log(x))[-1,]  # log returns and remove NA
    
  # Calculate historical VaR value and transform into matrix format
  x <- as.matrix(apply(x, 2, function(col) quantile(col, 1 - VaR * 0.01)))
    
  colnames(x) <- c(sprintf("VaR %s%%", VaR)) # Put name for column
    
  return(x) # Display matrix
}
histVaR(stock_data, 95) # Test
