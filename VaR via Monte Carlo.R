# Monte Function
monte_carlo_for_var <- function(c, ndays, n, VaR_for_monte = 95){
  
  # Set list to store values
  list_var_mc <- NULL
  
  # Set title for plot
  title_for_var <- colnames(c)
  
  # For each column in data set
  for (b in 1:ncol(c)){
    
    # Define name for column variable
    security <- c[,b]
    
    # Calculate return
    lrtn <- as.numeric(security / lag(security))
    
    # Define first value in column as 1
    lrtn[1] <- 1
    
    # Calculate various scenarios of Stock Performance
    set.seed(0)
    
    # Mimic Historical Performance using log returns
    paths <- replicate(n, expr = round(sample(lrtn,ndays,replace = TRUE),2))
    
    # Put values into list and calculate cumulative sums
    paths <- apply(paths, 2, cumprod)
    
    # Transform it into Time Series
    paths <- data.table(paths)
    paths$days <- 1:nrow(paths)
    paths <- melt(paths,
                  id.vars = "days")
    
    # Make Line Charts with all scenarios
    monte_graph <- ggplot(paths,
                          aes(x = days, y = (value - 1) * 100,
                              col = variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(title_for_var) +
      xlab("Days Invested") + 
      ylab("Portfolio Return (%)")
    
    # Calculate VaR and add to list
    list_var_mc <- rbind(list_var_mc, 
                         quantile(((paths$value[paths$days == ndays] - 1) *
                                     100), 1 - VaR_for_monte * 0.01) / ndays)
  }
  # Give row name
  rownames(list_var_mc) <- title_for_var
  
  # Give column name
  colnames(list_var_mc) <- "VaR MC"
  
  # Display values
  return(list_var_mc)
}
# Test
monte_carlo_for_var(portfolioReturns, 252, 100)
