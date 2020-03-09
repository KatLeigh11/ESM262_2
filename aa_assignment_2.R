#price data frame
species <- c("Scads", "Yellowstripe", "Mackerel", "Anchovies", "Sardinellas", "Ponyfishes", "Catfish")

prices <-  c(2, 5, 3, 1, 3, 6, 5)

price_df <- data.frame(species, prices)


#adding location and counts to data table
usa <- c(20, 15, 30, 50, 45, 5, 10)
indonesia <- c(100, 150, 300, 450, 250, 150, 50)
philippines <- c(80, 70, 60, 65, 75, 25, 10)

fish_data <- data.frame(species, usa, indonesia, philippines, prices) %>% 
  mutate(
    usa = as.numeric(usa),
    indonesia = as.numeric(indonesia),
    philippines = as.numeric(philippines)
  )

summary(fish_data)

fish_function<- function (species) {
  species = as.factor(species)
  max = which.max(summary(species))
  dominant = names(summary(species)[max])
  return(list(max = max, dominant = dominant))
}


fish_function(fish_data)
