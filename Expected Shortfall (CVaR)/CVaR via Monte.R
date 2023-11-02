lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"), # Libraries
       require, character.only = TRUE)

# Monte Function for Expected Shortfall
monte.carlo.es <- function(c, ndays, n, ES = 95){ l <- NULL
  
  # For each column in data set
  for (b in 1:ncol(c)){ v <- as.numeric(c[,b] / lag(c[,b])) # Calculate return
    
    v[1] <- 1 # Define first value in column as 1
    
    set.seed(0) # Calculate various scenarios of Stock Performance
    
    # Mimic Historical Performance & calculate cumulative sums
    p <- apply(replicate(n,expr=round(sample(v,ndays,replace=T),2)),2,cumprod)
    
    # Transform it into Time Series
    p <- data.table(p)
    p$days <- 1:nrow(p)
    p <- melt(p, id.vars = "days")
    
    # Calculate CVaR and add to list
    l <- rbind(l, mean(quantile(((p$value[p$days == ndays] - 1) * 100),
                                probs = seq(0, 1 - CVaR * .01, 1/n)/ndays))) }
  
  rownames(l) <- colnames(c) # Give row name
  colnames(l) <- "ES MC" # Give column name
  
  return(l) # Display values
}
# Test
monte.carlo.es(portfolioReturns, ndays = 252, n = 100)
