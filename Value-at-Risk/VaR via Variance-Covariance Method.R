# Function to calculate VaR via Variance Covariance Method
var_vc <- function(x, y = 95, lg = T){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) { print("Insufficient number of observations.") } else {
  
    # Calculate log returns and remove NA if necessary
    if (isTRUE(lg)) { x <- diff(log(x))[-1,] }
    
    # Calculate means and SDs
    vs_VaR <- apply(x, 2, function(x) c(mean(x), sd(x)))
    
    # Set up list to contain future values
    var_vc <- NULL
    
    # For each asset
    for (n in 1:ncol(x)){ 
      
      # Calculate VaR using standard norm probs and join to list
      var_vc <- rbind(var_vc, vs_VaR[1,n] + qnorm(1 - y * 0.01) * vs_VaR[2,n])}
    
    # Return names to assets
    rownames(var_vc) <- colnames(x)
    
    # Name parameter
    colnames(var_vc) <- sprintf("VaR VC %s%%", y) }
  
  # Display values
  return(var_vc)
}
# Test
var_vc(stock_data, 95)
