# Function to calculate VaR via Variance Covariance Method
var_via_sd <- function(x, y = 95){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) {
    print("Error. Insufficient number of observations for analysis.")
  } else {
    
    # Calculate log returns
    x=diff(log(x))[-1,]
    
    # Calculate means
    mean_VaR <- apply(x, 2, function(col) mean(col))
    
    # Calculate SDs
    sd_VaR <- apply(x, 2, function(col) sd(col))
    
    # Find quantile's value from Table of Standard Normal Probabilities
    norm_VaR <- qnorm(1 - y * 0.01)
    
    # Set up list to contain future values
    var_sd_list <- NULL
    
    # For each asset
    for (n in 1:ncol(x)){
      
      # Calculate VaR and join to list
      var_sd_list <- rbind(var_sd_list, mean_VaR[n] + norm_VaR * sd_VaR[n])
    }
  
    # Give names for columns
    rownames(var_sd_list) <- colnames(x)
    
    # Name parameter
    colnames(var_sd_list) <- "VaR V-C"
  }
  # Display values
  return(var_sd_list)
}
# Test
var_via_sd(stock_data, 99)
