# Libraries
lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T)

# Monte Function
monte.carlo.var <- function(c, ndays, n, VaR = 95){
  
  l.var <- NULL # Set list to store values
  
  # For each column in data set
  for (b in 1:ncol(c)){ lrtn <- as.numeric(c[,b] / lag(c[,b]))
    
    lrtn[1] <- 1 # Define first value in column as 1
    
    set.seed(0) # Calculate various scenarios of Stock Performance
    paths <- replicate(n, expr = round(sample(lrtn, ndays, replace = T), 2))
    paths <- data.table(apply(paths, 2, cumprod)) # Calculate cumulative sums
    paths$days <- 1:nrow(paths)
    paths <- melt(paths, id.vars = "days")
    
    l.var <- rbind(l.var, quantile(((paths$value[paths$days==ndays]-1)*100),
                                   1 - VaR * 0.01))} # Add VaR to list
  # Give row and column names
  rownames(l.var) <- colnames(c)
  colnames(l.var) <- "VaR MC"
  
  return(l.var) # Display values
}
# Test
monte.carlo.var(portfolioReturns, 252, 100)
