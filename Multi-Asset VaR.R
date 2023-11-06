# Function to calculate Multi-Asset Value-at-Risk
MVaR <- function(x, VaR = 0.01, p.weights, lg = T, ndays = 1){
  
  if (isTRUE(lg)) { x = diff(log(x))[-1,] } # Make logs if needed
  
  # Calculate sum of products for means and weights
  r.pd <-crossprod(x=as.matrix(apply(x,2,function(col) mean(col))),y=p.weights)
  
  # Calculate sum of products for variances and square weights
  v.pd<-crossprod(x=as.matrix(apply(x,2,function(col) var(col))),y=p.weights^2)
  
  a.names <- colnames(x) # Assign names
  
  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(0,nrow=length(a.names), ncol=length(a.names))
  
  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
      weight_product_matrix[i, j] <- p.weights[i] * p.weights[j] } }
      
  # Set row and column names
  rownames(weight_product_matrix) <- a.names
  colnames(weight_product_matrix) <- a.names
  
  # Put weights and covariances in nested list
  list_wghts_cov <- list(weight_product_matrix, cov(x))
  
  wghts_cov <- NULL # Create an empty variable to put covariances and weights
  
  for (n in 1:length(list_wghts_cov)){ # For each table in tables
    
    # Extract unique pairs and their correlations
    cor_pairs <- which(upper.tri(list_wghts_cov[[n]], diag = T), arr.ind = T)
    
    # Put them into one data frame
    unique_pairs <- data.frame(
      Variable1 = rownames(list_wghts_cov[[n]])[cor_pairs[, 1]],
      Variable2 = rownames(list_wghts_cov[[n]])[cor_pairs[, 2]],
      Correlation = (list_wghts_cov[[n]])[cor_pairs]
    )
    
    # Filter out pairs where the tickers are not the same
    different_tickers_pairs <- unique_pairs[unique_pairs$Variable1 !=
                                              unique_pairs$Variable2, ]
    
    # Concatenate First_Name and Last_Name with a space in between
    different_tickers_pairs$Pair <- paste(different_tickers_pairs$Variable1,
                                          different_tickers_pairs$Variable2)
    
    # Create new tables where put pairs in first column and values in second
    new_data_set <- as.data.frame(different_tickers_pairs[,4:3])
    
    # When working with first table assign to empty variable
    if (is.null(wghts_cov)){ wghts_cov <- new_data_set } else {
      
      # Otherwise perform inner join
      wghts_cov_join <- merge(x = wghts_cov, y = new_data_set, by = "Pair")} }
  
  # Sum of products for covariances and weights
  cov_prod <- crossprod(x = wghts_cov_join[,2], y = wghts_cov_join[,3]) * 2
  
  p.risk <- (v.pd + cov_prod) ^ .5 # Portfolio's standard deviation
  
  # Final calculation
  VaR_value <- round((1 - qnorm(VaR,mean=1 + r.pd,sd=p.risk))*ndays^.5,4) * 100
  
  # Prepare it for presentation
  sprintf("Portfolio MVaR is %s%% for the next %s days.", VaR_value, ndays)
}
# Test
MVaR(x = stock_data, VaR=.01, p.weights=c(.4, .3, .2, .1), lg = T, ndays = 10)
