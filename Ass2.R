library(tidyverse)
library(tidyr)

# create price dataframe

species <- c("Scads", "Yellowstripe", "Mackerel", "Anchovies", "Sardinellas", "Ponyfishes", "Catfish")

prices <-  c(2, 5, 3, 1, 3, 6, 5)

df_price <- data.frame(species, prices)

# create count dataframe

countriez <- c("USA", "USA", "USA", "USA", "USA", "USA", "USA", "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Indonesia", "Philippines", "Philippines", "Philippines", "Philippines", "Philippines", "Philippines", "Philippines")

countz <- c(20, 15, 30, 50, 45, 5, 10, 100, 150, 300, 450, 250, 150, 50, 80, 70, 60, 65, 75, 25, 10)

df_counts <- data.frame(countriez, countz)

df_counts

fishy_FUNction <- function(prices, counts) {
  
  fcountz <- df_counts %>%
    group_by(countriez) %>% 
    summarize(sum(countz))
    
  frev <- df_counts %>% 
    group_by(countriez) %>% 
    mutate(rev = countz * df_price$prices)
  
  frevloc <-frev %>% 
    summarise(Revenue = sum(rev))
  
  totfrev 
  fgraph
