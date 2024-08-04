Rachev.ratio <- function(x, VaR, lg = T){ # Rachev Ratio via historical method
  
  if (nrow(x) < 100) { print("Error. Insufficient number of observations.") } 
  
    else { if (isTRUE(lg)) { x <- diff(log(x))[-1,] } # Make log if needed
    
    L <- NULL
    
    for (m in 1:length(VaR)){ l <- NULL # Sort in ascending way
    
      # Calculate mean of both best and worst 5% observations and divide
      for (n in 1:ncol(x)){ es <- x[,n][order(x[,n]),]
      
        l <- rbind(l, round(mean(es[(VaR[m] * .01 * nrow(es)):nrow(es),]) /
                              -mean(es[1:((1 - VaR[m]*.01)*nrow(es)),]), 3)) } 
      
      colnames(l) <- sprintf("Rachev Ratio at %s%%", VaR[m]) # Rachev Ratio %
      rownames(l) <- colnames(x) # Assign tickers
      
      L <- cbind(L, l) } # Join
  
  L } # Display data frame 
}
Rachev.ratio(stock_data, c(95, 97.5, 99)) # Test
