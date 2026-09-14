# Libraries
lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T)

monte.carlo.var <- function(x, ndays, n, VaR = 95, yahoo=T){ # Monte Function
  
  if (yahoo){ P <- NULL # When Data from Yahoo! Finance needed
    
    for (A in 1:length(x)){ s = getSymbols(x[A],src="yahoo",auto.assign=F)[,4] 
    
      message(
        sprintf(
          "%s is downloaded; %s from %s", 
          x[A], which(x == x[A]), length(x)
        )
      )
      
      s <- s[apply(s, 1, function(x) all(!is.na(x))),] # Reduce NA
      
      colnames(s) <- x[A] 
      
      if (!is.timeSeries(s)){ s <- as.timeSeries(s) }
      
      if (is.null(P)) P <- list(s) else P[[A]] <- s } }
      
  L <- NULL
  
  for (m in 1:length(P)){ c <- P[[m]]
    
    r <- as.numeric(c / lag(c)) # Calculate returns
    r[1] <- 1 # Assign first observation as 1
    set.seed(0) # Calculate various scenarios of Stock Performance
    
    # Mimic Historical Performance using log returns
    p <- data.table(
      apply(
        replicate(n, expr = round(sample(r, ndays, replace=T), 2)),
        2,
        cumprod
      )
    )
    
    p$days <- 1:nrow(p)
    p <- melt(p, id.vars = "days")
    
    L <- rbind(
      L, 
      quantile(
        (p$value[p$days == ndays] - 1) * 100,
        1 - VaR * 0.01
        )
      ) # Add VaR to list
  }
  
  rownames(L) <- x
  colnames(L) <- "VaR MC (%)"
  
  L # Display values
}
monte.carlo.var(c("GOOGL", "AMZN"), 250, 100) # Test
