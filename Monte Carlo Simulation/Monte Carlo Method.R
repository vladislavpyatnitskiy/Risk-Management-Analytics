lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T) # Libraries

monte.carlo <- function(x, ndays, n, yahoo = T){ # Monte Carlo Simulation
  
  if (isTRUE(yahoo)){ P <- NULL # When Data from Yahoo! Finance needed
  
    for (A in x){ P <- cbind(P, getSymbols(A,src="yahoo",auto.assign=F)[,4]) }
  
    P <- P[apply(P, 1, function(x) all(!is.na(x))),] # Reduce NA
    
    colnames(P) <- x } else { P <- x } # Assign columns
  
  c <- as.timeSeries(P) # Make data Time Series
  
  r <- as.numeric(c / lag(c)) # Calculate returns
  r[1] <- 1 # Assign first observation as 1
  set.seed(0) # Calculate various scenarios of Stock Performance
  
  # Mimic Historical Performance using log returns
  p <- data.table(apply(replicate(n,expr=round(sample(r,ndays,replace=T),
                                               2)),2,cumprod))
  p$days <- 1:nrow(p)
  p <- melt(p, id.vars = "days")
  
  # Make Line Charts with all scenarious
  monte_graph <- ggplot(p, aes(x=days,y=(value - 1) * 100, col=variable)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(sprintf("%s Performance by Monte Carlo Simulation", colnames(c))) +
    xlab("Days Invested") + 
    ylab("Return (%)")
  
  monte_summary <- summary((p$value[p$days == ndays] - 1) * 100) # Stats
  
  monte_mean <- mean((p$value[p$days] - 1) * 100 < 0) # Expected Return
  
  list(monte_graph, monte_summary, monte_mean) # plot & stats
}
monte.carlo("GOOGL", 1000, 100) # Test
