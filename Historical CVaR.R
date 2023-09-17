histCVaR <- function(x, y){
  # Check whether there are less than 100 observations
  if (nrow(x) < 100) {
    print("Error. Insufficient number of observations for analysis.")
  } else {
    # Calculate its quantile
    CVaR_y <- 1 - y * 0.01
    
    # Calculate log returns and remove NA
    x <- diff(log(x))[-1,]
    
    names_for_cvar <- colnames(x)
    
    list_for_cvar <- NULL
    
    for (n in 1:ncol(x)){
      
      testcvar <- x[,n][order(x[,n]),]
      
      number_for_cvar <- testcvar[1:(0.05 * nrow(testcvar)),]
      
      cvar_value <- mean(number_for_cvar)
      
      list_for_cvar <- c(list_for_cvar, cvar_value)
    }
    
    # Transform into matrix format
    list_for_cvar <- as.matrix(list_for_cvar)
    
    # Put name for column
    colnames(list_for_cvar) <- c(sprintf("CVaR %s", y))
    
    rownames(list_for_cvar) <- names_for_cvar
    
    # Display matrix
    return(list_for_cvar)
  }
}
histCVaR(stock_data, 95)
