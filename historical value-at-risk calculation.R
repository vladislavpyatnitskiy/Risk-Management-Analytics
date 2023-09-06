# Function to find values for Value-at-Risk (VaR)

histVaR <- function(x, y){
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) {
    print("Error. Insufficient number of observations for analysis.")
  } else {
    # Calculate its quantile
    VaR_y <- 1 - y * 0.01
    
    # Calculate historical VaR value 
    x <- apply(x,
          2,
          function(col) quantile(col, VaR_y))
    
    # Transform into matrix format
    x <- as.matrix(x)
    
    # Put name for column
    colnames(x) <- c(sprintf("VaR %s", y))
  
    # Display matrix
    return(x)
  }
}
#Test new function
histVaR(lrtn, 95)
