# Calculate VaR via Historical Method
histVaR <- function(x, VaR, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Insufficient number of observations.") } else {
    
    # Calculate log returns and remove NA if necessary
    if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
    
    # Calculate historical VaR value and transform into matrix format
    x <- as.matrix(apply(x, 2, function(col) quantile(col, 1 - VaR * 0.01)))
    
    # Put name for column
    colnames(x) <- c(sprintf("VaR %s%%", VaR))
    
    # Display matrix
    return(x) }
}
# Test
histVaR(stock_data, 95)
