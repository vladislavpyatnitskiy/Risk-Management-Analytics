lapply(
  c("quantmod", "ggplot2", "data.table", "timeSeries"), 
  require, character.only = T
  ) # Libraries

monte.carlo <- function(x, ndays, n, yahoo = T){ # Monte Carlo Simulation
  
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
  Av <- NULL
  Plots <- NULL
  
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
    
    # Make Line Charts with all scenarios
    plt <- ggplot(p, aes(x=days,y=(value - 1) * 100, col=variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(sprintf("%s Monte Carlo Simulation Performance", colnames(c))) +
      xlab("Days Invested") + 
      ylab("Return (%)")
    
    summary <- as.vector(summary((p$value[p$days == ndays] - 1) * 100)) 
    
    names(summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    
    L <- rbind(L, t(as.data.frame(summary))) # Join
    
    Av <- c(Av, as.vector(mean((p$value[p$days] - 1) * 100 < 0))) # Join
    
    if (is.null(Plots)) Plots <- list(plt) else Plots[[m]] <- plt }
    
  names(Plots) <- x
  names(Av) <- x # Assign names
  rownames(L) <- x # Assign row names
  
  L <- as.data.frame(L)
  
  L <- L[order(-L$`Median`), ] # Sort by yield level
  Av <- sort(Av, decreasing = F)
  
  DF <- list(Plots, L, Av) # Output
  
  names(DF) <- c("Plots", "Yield", "Means")
  
  DF
}    
monte.carlo(c("GOOGL", "AMZN"), 1000, 100) # Test
