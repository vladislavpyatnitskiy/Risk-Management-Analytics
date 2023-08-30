# Monte Carlo code
lapply(c("quantmod",
         "ggplot2",
         "data.table",
         "timeSeries"),
       require,
       character.only = TRUE)

# Securities to analyse
tickers <- c("AMR")

# Data Extraction
portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(portfolioPrices,
                           getSymbols(Ticker,
                                      src = "yahoo",
                                      auto.assign=FALSE)[,4])
portfolioPrices <- portfolioPrices[apply(portfolioPrices,
                                         1,
                                         function(x) all(!is.na(x))),]
colnames(portfolioPrices) <- tickers

# Make it Time Series
portfolioReturns <- ROC(portfolioPrices,
                        type = "discrete")
portfolioReturns <-as.timeSeries(portfolioPrices)

# Monte Function
monte_carlo <- function(c, ndays, n){
  # Calculate returns
  lrtn <- c / lag(c)
  lrtn <- as.numeric(lrtn)
  lrtn[1] <- 1
  
  # Calculate various scenarios of Stock Performance
  set.seed(0)
  paths <- replicate(n, 
                     expr = round(sample(lrtn,
                                         ndays,
                                         replace = TRUE),
                                  2))
  paths <- apply(paths,
                 2,
                 cumprod)
  
  # Transform it into Time Series
  paths <- data.table(paths)
  paths$days <- 1:nrow(paths)
  paths <- melt(paths,
                id.vars = "days")
  
  # Make Line Charts with all scenarious
  monte_graph <- ggplot(paths, aes(x = days,
                    y = (value - 1) * 100,
                    col = variable)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    xlab("Days Invested") + 
    ylab("Portfolio Return (%)")
  
  # Matrix of Statistics
  monte_summary <- summary((paths$value[paths$days == ndays] - 1) * 100)
  
  # Mean (Expected) Return  
  monte_mean <- mean((paths$value[paths$days] - 1) * 100 < 0)
  
  # Combine all results for representation
  out <- list(monte_graph, monte_summary, monte_mean)
  
  # Show
  return(out)
}

monte_carlo(portfolioReturns, 1000, 100)
