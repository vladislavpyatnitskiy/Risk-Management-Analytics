lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T) # Libraries

monte.carlo.table <- function(x, ndays, n){ # Table for Monte values
  
  P <- NULL # When Data from Yahoo! Finance needed
  
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
    
    if (is.null(P)) P <- list(s) else P[[A]] <- s }
    
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
    
    summary <- as.vector(summary((p$value[p$days == ndays] - 1) * 100)) 
    
    names(summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    
    L <- rbind(L, t(as.data.frame(summary))) } # Join
    
  rownames(L) <- x # Assign row names
  
  L <- as.data.frame(L)
  
  L[order(-L$`Median`), ] # Sort by yield level
}
monte.carlo.table(c("UNM", "MET", "AIG", "OMF", "HIG"), 1000, 100) # Test
