# Monte Carlo code
lapply(c("quantmod", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T)

# Securities to analyse
tickers <- c("AMR")

portfolioPrices <- NULL
for (Ticker in tickers) 
  portfolioPrices <- cbind(p.Prices, getSymbols(Ticker, src = "yahoo",
                                                auto.assign=F)[,4])
p.Prices <- p.Prices[apply(p.Prices, 1, function(x) all(!is.na(x))),]
colnames(portfolioPrices) <- tickers

# Make it Time Series
portfolioReturns <-as.timeSeries(p.Prices)

# Monte Function
monte_carlo <- function(c, ndays, n){
  
  lrtn <- as.numeric(c / lag(c)) # Calculate returns
  lrtn[1] <- 1 # Assign first observation as 1
  set.seed(0) # Calculate various scenarios of Stock Performance
  
  # Mimic Historical Performance using log returns
  paths <- replicate(n, expr = round(sample(lrtn, ndays, replace = TRUE), 2))
  
  # Put values into list and calculate cumulative sums
  paths <- apply(paths, 2, cumprod)
  
  # Transform it into Time Series
  paths <- data.table(paths)
  paths$days <- 1:nrow(paths)
  paths <- melt(paths, id.vars = "days")
  
  # Make Line Charts with all scenarious
  monte_graph <- ggplot(paths, aes(x=days,y=(value - 1) * 100, col=variable)) +
    geom_line() +
    theme_bw() +
    theme(legend.position = "none") +
    ggtitle(sprintf("%s Performance by Monte Carlo Simulation", colnames(c))) +
    xlab("Days Invested") + 
    ylab("Return (%)")
  
  monte_summary <- summary((paths$value[paths$days == ndays] - 1) * 100) #Stats
  
  monte_mean <- mean((paths$value[paths$days] - 1) * 100 < 0) #Expected Return
  
  list(monte_graph, monte_summary, monte_mean) # plot & stats
}
# Test
monte_carlo(portfolioReturns, 1000, 100)
