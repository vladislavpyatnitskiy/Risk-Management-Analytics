# Value-at-Risk via Variance-Covariance Method
var_via_sd <- function(x){

  # Calculate log returns and remove NAs
  x=diff(log(x))[-1,]
  
  # Calculate means
  mean_for_var <- apply(x, 2, function(col) mean(col))
  
  # Calculate SDs
  sd_for_var <- apply(x, 2, function(col) sd(col))
  
  # Set up list to contain future values
  var_sd_list <- NULL
  
  # Give names for columns
  names_for_var_sd <- colnames(x)
  
  # For each asset
  for (n in 1:(ncol(x))){
    
    # Calculate VaR 
    var_sd_coef <- (mean_for_var[n]) - 1.645 * (sd_for_var[n])
    
    # Join to list
    var_sd_list <- cbind(var_sd_list, var_sd_coef)
  }
  # Transform to matrix
  var_sd_list <- as.matrix(var_sd_list)
  
  # Transpose
  var_sd_list <- t(var_sd_list)
  
  # Return names to assets
  rownames(var_sd_list) <- names_for_var_sd
  
  # Name parameter
  colnames(var_sd_list) <- "VaR V-C"
  
  # Display values
  return(var_sd_list)
}
# Test
var_via_sd(stock_data)
