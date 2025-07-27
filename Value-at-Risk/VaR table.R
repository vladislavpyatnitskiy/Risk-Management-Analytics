library("data.table") # Libraries

# Table with VaRs
VaR.t <- function(data, VaR = 95, ndays = 252, q = 100, lg = T){
  
  ### Historical VaR Part ###
  
  # Check whether there are less than 100 observations
  if (nrow(data) < 100) return(message("Insufficient number of observations."))
    
  if (lg) { var_x1 <- diff(log(data))[-1,] } # Log returns & NA off
    
  y = 1 - VaR * 0.01 # Calculate VaR's quantile
    
  hist_x <- apply(var_x1, 2, function(col) quantile(col,y)) # Historical VaR
    
  ### VaR Variance Covariance Part ###
    
  v.VaR <- apply(var_x1, 2, function(x) c(mean(x), sd(x))) # Means and SD
    
  vc <- NULL # Set up list to contain future values
    
  # For each asset calculate VaR using standard norm probs and join to list
  for (n in 1:ncol(data)){ vc <- rbind(vc, v.VaR[1,n] + qnorm(y)*v.VaR[2,n]) }
    
  ### VaR by Monte Carlo ###
    
  mc <- NULL # Set list to store values
    
  # Calculate returns
  for (b in 1:ncol(data)){ v <- as.numeric(data[,b] / lag(data[,b]))
      
    v[1] <- 1 # Assign 1 as a first value
      
    set.seed(0) # Calculate various scenarios of Stock Performance
      
    # Mimic Historical Performance using log returns
    p <- replicate(q, expr = round(sample(v, ndays, replace = T), 2))
      
    # Put values into list and calculate cumulative sums
    p <- data.table(apply(p, 2, cumprod))
    p$days <- 1:nrow(p)
    p <- melt(p, id.vars = "days")
      
    # Calculate VaR and add to list
    mc <- rbind(mc, quantile(((p$value[p$days == ndays] - 1)*100),y)/ndays) }
      
  ### End of main calculations ###
    
  new_var_list <- data.frame(hist_x, vc, mc) # Join three matrices 
    
  colnames(new_var_list) <- c("VaR HM", "VaR VC", "VaR MC") # Column names
    
  return(new_var_list) # Display matrix
}
VaR.t(data = stock_data, VaR = 95) # Test
