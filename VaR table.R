# Select Libraries
lapply(c("quantmod",
         "ggplot2",
         "data.table",
         "timeSeries"),
       require,
       character.only = TRUE)

# Table with VaRs
VaR_table <- function(var_x, VaR = 95, ndays = 252, n_obvs = 100, lg = T){
  
  ### Historical VaR Part ###
  
  # Check whether there are less than 100 observations
  if (nrow(var_x)<100) { print("Insufficient number of observations.") } else {
    
    # Calculate log returns and remove NA if necessary
    if (isTRUE(lg)) { var_x1 <- diff(log(var_x))[-1,] }
    
    # Calculate VaR's quantile
    y = 1 - VaR * 0.01
    
    # Calculate historical VaR value 
    hist_x <- apply(var_x1, 2, function(col) quantile(col, y))

    ### VaR Variance Covariance Part ###
    
    # Calculate means and SDs
    vs_VaR <- apply(var_x1, 2, function(x) c(mean(x), sd(x)))
    
    # Set up list to contain future values
    var_vc <- NULL
    
    # For each asset calculate VaR using standard norm probs and join to list
    for (n in 1:(ncol(var_x))){ var_vc <- rbind(var_vc,vs_VaR[1,n] + qnorm(y) *
                                                  vs_VaR[2,n])}
    ### VaR by Monte Carlo ###
    
    # Set list to store values
    list_var_mc <- NULL
    
    for (b in 1:ncol(var_x)){ security <- var_x[,b]
      
      # Calculate returns
      lrtn_for_monte <- as.numeric(security / lag(security))
      
      # Assign 1 as a first value
      lrtn_for_monte[1] <- 1
      
      # Calculate various scenarios of Stock Performance
      set.seed(0)
      
      # Mimic Historical Performance using log returns
      paths <- replicate(n_obvs, expr = round(sample(lrtn_for_monte, ndays,
                                             replace = TRUE),2))
      
      # Put values into list and calculate cumulative sums
      paths <- data.table(apply(paths,2,cumprod))
      paths$days <- 1:nrow(paths)
      paths <- melt(paths, id.vars = "days")
      
      # Calculate VaR and add to list
      list_var_mc <- rbind(list_var_mc, quantile(((paths$value[paths$days ==
                                                                 ndays] -
                                                     1) * 100), y) / ndays) }
    ### End of main calculations ###
    
    # Combine all three matrices into one
    new_var_list <- data.frame(hist_x, var_vc, list_var_mc)
    
    # Give column names
    colnames(new_var_list) <- c("VaR HM", "VaR VC", "VaR MC")
    
    # Display matrix
    return(new_var_list) }
}
# Test
VaR_table(var_x = stock_data, VaR = 95)
