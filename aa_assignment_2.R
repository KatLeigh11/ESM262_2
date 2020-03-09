library(tidyr)
library(tidyverse)

#price data frame
species <- c("Scads", "Yellowstripe", "Mackerel", "Anchovies", "Sardinellas", "Ponyfishes", "Catfish")

prices <-  c(2, 5, 3, 1, 3, 6, 5)

price_df <- data.frame(species, prices)

#adding location and counts to data table
usa <- c(20, 15, 30, 50, 45, 5, 10)
indonesia <- c(100, 150, 300, 450, 250, 150, 50)
philippines <- c(80, 70, 60, 65, 75, 25, 10)

counts_by_country <- data.frame(species, usa, indonesia, philippines) %>% 
  mutate(
    usa = as.numeric(usa),
    indonesia = as.numeric(indonesia),
    philippines = as.numeric(philippines)
  )

fish_data <- counts_by_country %>% 
  mutate(prices)

summary(fish_data)

pivotf <- pivot_longer(counts_by_country, c(usa, indonesia, philippines), names_to = "country", values_to = "count")

usaf <- pivotf %>% 
  filter(country == "usa") %>% 
  group_by(species) %>% 
  which.max(count)

indof <- pivotf %>% 
  filter(country == "indonesia") %>% 
  group_by(species) %>% 
  summarise(count)

philf <- pivotf %>% 
  filter(country == "philippines") %>% 
  group_by(species) %>% 
  summarise(count)

fish_function<- function (fcounts, fprices) {
  species = as.factor(counts_by_country$species)
  max1 = which.max(summary(counts_by_country$usa))
  max2 = which.max(summary(counts_by_country$indonesia))
  max3 = which.max(summary(counts_by_country$philippines))
  
  dominant = names(summary(counts_by_country$species)[max])
  return(list(max = max, dominant = dominant))
}

max

fish_function(fish_data)
