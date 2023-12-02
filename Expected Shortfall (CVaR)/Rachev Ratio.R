# Function to calculate Rachev Ratio via historical method
Rachev.ratio <- function(x, VaR, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Error. Insufficient number of observations.") } 
  
  # Otherwise calculate logs & remove NA if necessary & create list for CVaR
  else { if (isTRUE(lg)) { x <- diff(log(x))[-1,] } 
    
    l.minus <- NULL # list with negative CVaR values
    l.max <- NULL # list with positive CVaR values
    
    # For each column in dataset assign variable & Sort ascendingly
    for (n in 1:ncol(x)){ es <- x[,n][order(x[,n]),]
      
      # Select 5% worst and 5% best obs, find their means and add to list
      l.minus <- rbind(l.minus, mean(es[1:((1 - VaR * .01) * nrow(es)),])) 
      l.max <- rbind(l.max, mean(es[(VaR * .01 * nrow(es)):nrow(es),])) }
    
    rr.coef <- round(l.max / -l.minus, 3) # Calculate Rachev Ratio
    
    colnames(rr.coef) <- sprintf("Rachev Ratio at %s%%", VaR) # Name for column
    
    rownames(rr.coef) <- colnames(x) # Give them names from data set
    
    return(rr.coef) } # Display matrix
}
Rachev.ratio(stock_data, 95) # Test
