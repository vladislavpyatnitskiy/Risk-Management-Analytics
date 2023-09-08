# Select Libraries
lapply(c("quantmod",
         "ggplot2",
         "data.table",
         "timeSeries"),
       require,
       character.only = TRUE)

# Table with VaRs
VaR_table <- function(var_x, y = 95, ndays = 252, n_obvs = 100){
  
  ### Historical VaR Part ###
  
  # Check whether there are less than 100 observations
  if (nrow(var_x) < 100) {
    print("Error. Insufficient number of observations for analysis.")
  } else {
    # Calculate its quantile
    VaR_y <- 1 - y * 0.01
    
    # Calculate log returns and remove NA
    var_x1 <- diff(log(var_x))[-1,]
    
    # Calculate historical VaR value 
    hist_x <- apply(var_x1,
               2,
               function(col) quantile(col, VaR_y))
    
    # Transform into matrix format
    hist_x <- as.matrix(hist_x)
    
    # Put name for column
    colnames(hist_x) <- "VaR His"
    
    ### VaR Variance Covariance Part ###
    
    # Calculate means
    mean_for_var <- apply(var_x1, 2, function(col) mean(col))
    
    # Calculate SDs
    sd_for_var <- apply(var_x1, 2, function(col) sd(col))
    
    # Find value from Table of Standard Normal Probabilities
    norm_for_var <- qnorm(VaR_y)
    
    # Set up list to contain future values
    var_sd_list <- NULL
    
    # Give names for columns
    names_for_var_sd <- colnames(var_x)
    
    # For each asset
    for (n in 1:(ncol(var_x))){
      
      # Calculate VaR 
      var_sd_coef <- (mean_for_var[n]) + norm_for_var * (sd_for_var[n])
      
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
    colnames(var_sd_list) <- "VaR VC"
    
    ### VaR by Monte Carlo ###
    
    # Set list to store values
    list_var_mc <- NULL
    
    # Set title for plot
    title_for_var <- colnames(var_x)
    
    for (b in 1:(ncol(var_x))){
      
      # Calculate returns
      lrtn_for_monte <- (var_x[,b]) / (lag(var_x[,b]))
      lrtn_for_monte <- as.numeric(lrtn_for_monte)
      lrtn_for_monte[1] <- 1
      
      # Calculate various scenarios of Stock Performance
      set.seed(0)
      
      # Mimic Historical Performance using log returns
      paths <- replicate(n_obvs, 
                         expr = round(sample(lrtn_for_monte,
                                             ndays,
                                             replace = TRUE),
                                      2))
      # Put values into list and calculate cumulative sums
      paths <- apply(paths,
                     2,
                     cumprod)
      
      # Transform it into Time Series
      paths <- data.table(paths)
      paths$days <- 1:nrow(paths)
      paths <- melt(paths,
                    id.vars = "days")
      
      # Make Line Charts with all scenarios
      monte_graph <- ggplot(paths,
                            aes(x = days,
                                y = (value - 1) * 100,
                                col = variable)) +
        geom_line() +
        theme_bw() +
        theme(legend.position = "none") +
        ggtitle(title_for_var) +
        xlab("Days Invested") + 
        ylab("Portfolio Return (%)")
      
      # Calculate VaR 
      var_f <- quantile(((paths$value[paths$days == ndays] - 1) * 100),
                        VaR_y) / ndays
      
      # Add to list 
      list_var_mc <- cbind(list_var_mc, var_f)
    }
    # Make it matrix
    list_var_mc <- as.matrix(list_var_mc)
    
    # Transpose
    list_var_mc <- t(list_var_mc)
    
    # Give row name
    rownames(list_var_mc) <- title_for_var
    
    # Give column name
    colnames(list_var_mc) <- "VaR MC"
    
    ### End of main calculations ###
    
    # Combine all three matrices into one
    new_var_list <- data.frame(hist_x,
                                var_sd_list,
                                list_var_mc)
    
    # Display matrix
    return(new_var_list)
  }
}
# Test
VaR_table(var_x = stock_data, y = 95)
