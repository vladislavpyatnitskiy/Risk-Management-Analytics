# Function to calculate VaR via Variance Covariance Method
var_via_sd <- function(x, y = 95){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) {
    print("Error. Insufficient number of observations for analysis.")
  } else {
    
    # Calculate log returns
    x=diff(log(x))[-1,]
    
    # Calculate means
    mean_for_var <- apply(x, 2, function(col) mean(col))
    
    # Calculate SDs
    sd_for_var <- apply(x, 2, function(col) sd(col))
    
    # Calculate its quantile
    VaR_y <- 1 - y * 0.01
    
    # Find value from Table of Standard Normal Probabilities
    norm_for_var <- qnorm(VaR_y)
    
    # Set up list to contain future values
    var_sd_list <- NULL
    
    # Give names for columns
    names_for_var_sd <- colnames(x)
    
    # For each asset
    for (n in 1:(ncol(x))){
      
      # Calculate VaR 
      var_sd_coef <- (mean_for_var[n]) + norm_for_var * (sd_for_var[n])
      
      # Join to list
      var_sd_list <- rbind(var_sd_list, var_sd_coef)
    }
    # Transform to matrix
    var_sd_list <- as.matrix(var_sd_list)
    
    # Return names to assets
    rownames(var_sd_list) <- names_for_var_sd
    
    # Name parameter
    colnames(var_sd_list) <- "VaR V-C"
  }
  # Display values
  return(var_sd_list)
}
# Test
var_via_sd(stock_data, 99)
