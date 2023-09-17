# Function to get Expected Shortfall via historical method
histCVaR <- function(x, y){
  
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) {
    
    # Because with sample of less than 100 observations CVaR is irrelevant
    print("Error. Insufficient number of observations for analysis.")
    
    # When there are 100 and more observations
  } else {
    
    # Calculate its quantile
    CVaR_y <- 1 - y * 0.01
    
    # Calculate log returns and remove NA
    x <- diff(log(x))[-1,]
    
    # Get column names from data set
    names_for_cvar <- colnames(x)
    
    # Create variable to contain values for CVaR
    list_for_cvar <- NULL
    
    # For each column in dataset
    for (n in 1:ncol(x)){
      
      # Sort values ascendingly
      testcvar <- x[,n][order(x[,n]),]
      
      # Select ones that in 5% worst observations
      number_for_cvar <- testcvar[1:(CVaR_y * nrow(testcvar)),]
      
      # Find their mean
      cvar_value <- mean(number_for_cvar)
      
      # Add the value in list
      list_for_cvar <- c(list_for_cvar, cvar_value)
    }
    
    # Transform into matrix format
    list_for_cvar <- as.matrix(list_for_cvar)
    
    # Put name for column
    colnames(list_for_cvar) <- c(sprintf("CVaR %s", y))
    
    # Give them names frm data set
    rownames(list_for_cvar) <- names_for_cvar
    
    # Display matrix
    return(list_for_cvar)
  }
}
# Test
histCVaR(stock_data, 95)
