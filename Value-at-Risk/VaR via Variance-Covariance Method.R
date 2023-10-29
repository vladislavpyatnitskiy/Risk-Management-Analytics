# Function to calculate VaR via Variance Covariance Method
var_vc <- function(x, y = 95, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Insufficient number of observations.") } else {
  
    if (isTRUE(lg)) { x <- diff(log(x))[-1,] } # Log returns and remove NA
    
    vs_VaR <- apply(x, 2, function(x) c(mean(x), sd(x))) # Means and SDs
    
    var_vc <- NULL # Set up list to contain future values
    
    for (n in 1:ncol(x)){ # For each asset
      
      # Calculate VaR using standard norm probs and join to list
      var_vc <- rbind(var_vc, vs_VaR[1,n] + qnorm(1 - y * 0.01) * vs_VaR[2,n])}
    
    rownames(var_vc) <- colnames(x) # Return names to assets
    colnames(var_vc) <- sprintf("VaR VC %s%%", y) } # Name parameter
  
  # Display values
  return(var_vc)
}
# Test
var_vc(stock_data, 95)
