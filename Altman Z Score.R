library("rvest") # Library

altmanz.score <- function(x){ l <- NULL # Function to get Altman Z Score

  for (n in 1:length(x)){ y <- x[n] # Get data of Sector and Altman Z Score
  
    S <- read_html(sprintf("https://stockanalysis.com/stocks/%s/",
                           tolower(y))) %>% html_nodes('body') %>%
      html_nodes('main') %>% html_nodes('div') %>% html_nodes('a') 
    
    R <- read_html(sprintf("https://stockanalysis.com/stocks/%s/statistics/",
                           tolower(y))) %>% html_nodes('table') %>%
      html_nodes('tr') %>% html_nodes('td') %>% html_text()
    
    q <- S %>% html_attr('class') == "dothref text-default" # Clean data & Join
    
    A <- as.numeric(R[grep("Altman Z-Score", R) + 1]) # Altman Z Score
    
    if (is.na(A)){ Z <- NA } else { if (A > 2.6){ Z <- "Safe" }
      
      else if (A < 2.6 && A > 1.1){ Z <- "Grey" } else { Z <- "Distress" } }
      
    l <- rbind.data.frame(l,
                          cbind(S[grep("TRUE", q)] %>% html_text() %>% .[2],
                                A, Z)) } # Sector, Atlman and Zone
  rownames(l) <- x
  colnames(l) <- c("Sector", "Altman Z-Score", "Zone") # Colunm names
  
  l # Display
}
altmanz.score(c("C", "GOOGL", "MU")) # Test
