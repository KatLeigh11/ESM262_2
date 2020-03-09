library(tidyverse)

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
    philippines = as.numeric(philippines),
    species = as.factor(species)
  )

fish_pivot <- pivot_longer(fish_data, c(usa, indonesia, philippines), names_to = "country", values_to = "count")

frev <- df_counts %>%
  group_by(countriez) %>%
  mutate(rev = countz * df_price$prices)

summary(fish_data)

fish_function<- function (fish_data, fish_graph) {
  max_usa = max(usa)
  max_indo = max(indonesia)
  max_phil = max(philippines)
  dominant_usa = species[which(usa == max_usa)]
  dominant_indo = species[which(indonesia == max_indo)]
  dominant_phil = species[which(philippines == max_phil)]
  frev <- fish_pivot %>% 
    group_by(country) %>% 
    mutate(
      rev = count * prices) %>% 
    summarize(
      total = sum(rev)
    )
  total_usa = frev$total[1]
  total_indo = frev$total[2]
  total_phil = frev$total[3]
  rev_usa = frev$country[which(frev$total == total_usa)]
  rev_indo = frev$country[which(frev$total == total_indo)]
  rev_phil = frev$country[which(frev$total == total_phil)]
  totrev = sum(frev$total)
  locations <- c(frev$country, "All")
  total_all <- c(frev$total, totrev)
  plot_df <- data.frame(locations, total_all)
  fish_plot <- ggplot(plot_df, aes(x = locations, y = total_all))+
    geom_col()
  fish_graph = ifelse((fish_graph == 0), return("No graph"), fish_plot)
  return(list(max = c(max_usa, max_indo, max_phil), dominant = c(dominant_usa, dominant_indo, dominant_phil), revenues = c(total_usa, total_indo, total_phil), rcountries = c(rev_usa, rev_indo, rev_phil), fishery_total = totrev, fish_graph))
}

fish_function(fish_data, 0)







