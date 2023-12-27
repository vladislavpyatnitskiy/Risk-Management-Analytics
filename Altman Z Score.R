library("rvest")

altmanz.score <- function(x, tech = T){ # Altman Z Score
  
  bs <- sprintf("https://finance.yahoo.com/quote/%s/balance-sheet?p=%s", x, x)
  is <- sprintf("https://finance.yahoo.com/quote/%s/financials?p=%s", x, x)
  re <- sprintf("https://finance.yahoo.com/quote/%s/key-statistics?p=%s",x,x)
  
  page.bs <- read_html(bs) # Read HTML & extract Balance Sheet info
  page.is <- read_html(is) # Income Statement
  page.re <- read_html(re) # Payout Ratio
  
  price.yahoo1 <- page.bs %>% html_nodes('div') %>% .[[1]] -> tab.bs
  price.yahoo2 <- page.is %>% html_nodes('div') %>% .[[1]] -> tab.is
  price.yahoo3 <- page.re %>% html_nodes('div') %>% .[[1]] -> tab.re
  
  y <- tab.bs %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  u <- tab.is %>% html_nodes('div') %>% html_nodes('span') %>% html_text()
  w <- tab.re %>% html_nodes('tr') %>% html_nodes('td') %>% html_text()
  
  b <- w[grep("Payout Ratio ", w) + 1] # Take Payout Ratio & make numeric
  
  b <- as.numeric(read.fwf(textConnection(b), widths = c(nchar(b) - 1, 1),
                           colClasses = "character")[, 1]) / 100
  c <- NULL
  h <- NULL
  
  p <- c("Total Assets", "Total Liabilities Net Minority Interest",
         "Total Equity Gross Minority Interest", "Working Capital")
  
  r <- c("EBIT", "Total Revenue", "Net Income Common Stockholders")
  
  for (m in 1:length(r)){ q <- NULL
  
    for (n in seq(1)){ q <- cbind(q, u[grep(r[m], u) + n])
    
      o <- NULL
      
      if (length(q) > 1){  o<-c(o,q[1]) } else if (length(q) == 1) { o <- q } } 
      
    c <- rbind(c, o) }
  
  c <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", c)) # Reduce commas
  
  for (m in 1:length(p)){ v <- NULL
  
    for (n in seq(1)){ v <- cbind(v, y[grep(p[m], y) + n])
    
      w <- NULL
      
      if (length(v) > 1){ for (n in seq(0, 3, 1)) w <- c(w, v[1 + 2 * n]) 
      
    } else if (length(v) == 1) { w <- v } } 
    
    h <- rbind(h, w) }
  
  h <- gsub(",", "", gsub("([a-zA-Z]),", "\\1 ", h)) # Reduce commas
  
  A <- as.numeric(h[4]) / as.numeric(h[1]) # Working Capital / Total Assets
  B <- (1 - b) * as.numeric(c[3]) / as.numeric(h[1]) # Retention / Total Assets
  C <- as.numeric(c[1]) / as.numeric(h[1]) # EBIT / Total Assets
  D <- as.numeric(h[3]) / as.numeric(h[2]) # Equity / Total Liabilities
  E <- as.numeric(c[2]) / as.numeric(h[1]) # Total Revenue / Total Assets
  
  if (isFALSE(tech)){ a.r = round(1.2 * A + 1.4 * B + 3.3 * C + .6 * D + E, 2)
  
  if (a.r > 2.6){ sprintf("%s Altman Z Score is %s, what is safe", x, a.r)}
  
  else if (a.r < 2.6 && a.r > 1.1){
    
    sprintf("%s Altman Z Score is %s, what is in grey zone", x,a.r) } else {
      
      sprintf("%s Altman Z Score is %s, what is in distress zone", x, a.r) }
  
  } else { a.r = round(6.56 * A + 3.26 * B + 6.72 * C + 1.05 * D, 2) 
  
  if (a.r > 2.6){ sprintf("%s Altman Z Score is %s, what is safe", x, a.r)}
  
  else if (a.r < 2.6 && a.r > 1.1){
    
    sprintf("%s Altman Z Score is %s, what is in grey zone", x,a.r) } else {
      
      sprintf("%s Altman Z Score is %s, what is in distress zone", x, a.r) } }
}
altmanz.score("ZIM", tech = F) # Test
