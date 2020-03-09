# creating data to test fish function

# creating the price data frame
species <- c("Scads", "Yellowstripe", "Mackerel", "Anchovies", "Sardinellas", "Ponyfishes", "Catfish")
prices <-  c(2, 5, 3, 1, 3, 6, 5)
price_df <- data.frame(species, prices)

# creating the location and counts data frame
usa <- c(20, 15, 30, 50, 45, 5, 10)
indonesia <- c(100, 150, 300, 450, 250, 150, 50)
philippines <- c(80, 70, 60, 65, 75, 25, 10)

fish_counts <- data.frame(species, usa, indonesia, philippines) %>% 
  mutate(
    usa = as.numeric(usa),
    indonesia = as.numeric(indonesia),
    philippines = as.numeric(philippines),
    species = as.factor(species)
  )
