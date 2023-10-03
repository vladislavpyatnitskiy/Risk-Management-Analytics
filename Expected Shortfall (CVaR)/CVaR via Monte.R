# Monte Function
monte_carlo_for_cvar <- function(c, ndays, n, ES_for_monte = 95){
  
  # Set list to store values
  list_cvar_mc <- NULL
  
  # Set title for plot
  title_for_cvar <- colnames(c)
  
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
    
    # Calculate CVaR and add to list
    list_cvar_mc <- rbind(list_cvar_mc,
                          mean(quantile(((paths$value[paths$days ==
                                                        ndays] - 1) *
                                                         100),
                                        probs = seq(0,
                                                    1 - ES_for_monte * 0.01,
                                                    1/n) / ndays)))
  }
  # Give row name
  rownames(list_cvar_mc) <- title_for_cvar
  
  # Give column name
  colnames(list_cvar_mc) <- "ES MC"
  
  # Display values
  return(list_cvar_mc)
}
# Test
monte_carlo_for_cvar(portfolioReturns, 
                    ndays = 252, 
                    n = 100)
