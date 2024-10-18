VaR.VC <- function(x, VaR = 95, lg = T){ # VaR via Variance Covariance Method
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { message("Insufficient number of observations.") } else {
    
    if (isTRUE(lg)) { x <- diff(log(x))[-1,] } # Log returns and remove NA
    
    v <- apply(x,2,function(x) c(mean(x), sd(x))) # Means & Standard Deviations
    
    l <- NULL # Set up list to contain future values
    
    # Calculate VaR using standard norm probs and join to list
    for (n in 1:ncol(x)){ l <- rbind(l, v[1,n] + qnorm(1 - VaR*.01) * v[2,n])}
    
    rownames(l) <- colnames(x) # Return names to assets
    colnames(l) <- sprintf("VaR VC %s%%", VaR) # Name parameter
  
    return(l) } # Display values
}
VaR.VC(stock_data, 95) # Test
