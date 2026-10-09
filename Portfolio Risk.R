lapply(c("quantmod", "timeSeries"), require, character.only = T) # lib

# Function to calculate portfolio risk
portfolio_risk <- function(x, s=NULL, e=NULL, p.weights, sqrt = T){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  src <- "yahoo"
  
  getData <- function(A, s, e) {
    if (is.null(s) && is.null(e)) return(getSymbols(A, src=src, auto.assign=F)) 
    if (is.null(e)) return(getSymbols(A, from = s, src=src, auto.assign=F)) 
    if (is.null(s)) return(getSymbols(A, to = e, src=src, auto.assign=F)) 
    return(getSymbols(A, from = s, to = e, src=src, auto.assign=F)) 
  }
  for (A in x){ p <- cbind(p, getData(A, s, e)[,4]) 
  
    message(sprintf("%s is downloaded; %s from %s", A, which(x==A), length(x)))
  
  } # Join data
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in column names
  
  p <- as.timeSeries(p) # Make it time series and display
  
  p = diff(log(p))[-1,]
  
  # Calculate sum of products for variances and weights
  V <- crossprod(
    x = as.matrix(apply(p, 2, function(col) var(col))),
    y = p.weights ^ 2
    )
  
  a.names <- colnames(p) # Assign names
  
  # Create a matrix to store the products of weights
  weight_product_matrix = matrix(0, nrow=length(a.names), ncol=length(a.names))
  
  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
    weight_product_matrix[i, j]<-p.weights[i] * p.weights[j] } }
  
  rownames(weight_product_matrix) <- a.names # Row Names
  colnames(weight_product_matrix) <- a.names # Column Names
  
  # Put weights and covariances in nested list
  list_wghts_cov <- list(weight_product_matrix, cov(p))
  
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
    different_tickers_pairs <- unique_pairs[
      unique_pairs$Variable1 != unique_pairs$Variable2, 
      ]
    
    # Concatenate First_Name and Last_Name with a space in between
    different_tickers_pairs$Pair <- paste(
      different_tickers_pairs$Variable1,
      different_tickers_pairs$Variable2
      )
    
    # Matrix with values of unique pairs
    new_data_set <- as.data.frame(different_tickers_pairs[,4:3]) 
    
    # Put values into table
    if (is.null(wghts_cov)){ wghts_cov <- new_data_set } else {
      
      wghts_cov_join <- merge(x = wghts_cov, y = new_data_set, by = "Pair")} }
  
  # Sum of products for covariances and weights
  CV <- crossprod(x = wghts_cov_join[,2], y = wghts_cov_join[,3]) * 2
  
  # Portfolio's standard deviation & Portfolio's variance
  if (sqrt) RISK = (V + CV) ^ .5 else RISK = V + CV
  
  RISK # Print the matrix
}
portfolio_risk(
  x = c("AIG", "AMZN", "XOM", "TSN"), p.weights = c(.4, .3, .2, .1), sqrt=T
  ) # Test
