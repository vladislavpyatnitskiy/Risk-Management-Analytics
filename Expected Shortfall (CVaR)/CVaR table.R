library("data.table") # Libraries

ES.t <- function(x, es = 95, ndays = 252, q = 100, lg = T){ # Table with CVaR
  
  # Check whether there are less than 100 observations
  if (nrow(x)<100) { print("Insufficient number of observations.") } else {
    
    # Calculate log returns and remove NA if necessary
    if (isTRUE(lg)) { a <- diff(log(x))[-1,] } # Log returns & NA off
    
    y = 1 - es * .01 # Calculate CVaR's quantile

    c <- apply(a,2,function(x) c(mean(x), sd(x))) # Means & Standard Deviations
    
    hs <- NULL # Create variable to contain values for CVaR
    vc <- NULL # Set up list to contain future values
    mc <- NULL # Set list to store values
    
    # For each column in data set
    for (b in 1:ncol(x)){ s <- a[,b] ### Historical Method ###
      
      hs <- rbind(hs, mean(s[order(s),][1:(y * nrow(s[order(s),])),]))
    
      ### CVaR Variance Covariance Part ###  
      vc <- rbind(vc, c[1,b] - dnorm(qnorm(es * .01)) / y * c[2,b])
      
      v <- as.numeric(x[,b] / lag(x[,b])) ### CVaR by Monte Carlo ###
      
      v[1] <- 1 # Define first value in column as 1
      
      set.seed(0) # Calculate various scenarios of Stock Performance
      
      # Mimic Historical Performance & calculate cumulative sums
      p<-apply(replicate(n,expr=round(sample(v,ndays,replace=T),2)),2,cumprod)
      
      # Transform it into Time Series
      p <- data.table(p)
      p$days <- 1:nrow(p)
      p <- melt(p, id.vars = "days")
      
      # Calculate CVaR and add to list
      mc <- rbind(mc, mean(quantile(((p$value[p$days == ndays] - 1) * 100),
                                        probs=seq(0,y, 1/n))/q)) }
    
    l.es <- data.frame(hs, vc, mc) # Join
    
    rownames(l.es) <- colnames(x) # Give them names from data set
    colnames(l.es) <- c(sprintf("ES HM %s%%", es), sprintf("ES VC %s%%", es),
                        sprintf("ES MC %s%%", es))
    
    return(l.es) } # Display matrix
}
ES.t(x = stock_data, es = 95, lg = T) # Test
