#' When given a dataframe containing 3 locations, this function returns the most frequently caught fish in each location , the revenue by location, and the total revenue across all locations. This function requires the tidyr and tidyverse libraries.
#' 
#' @param fish_counts a table containing the number of fish caught for each species at each location. The first column contains the species, with each subsequent column named the location and filled with the corresponding fish counts. Each row is a different fish species.
#' @param price_df a price table listing fish species with "species" and "prices" as column names
#' @param fish_graph a binary option to display a plot of fishery revenue by location in text format, default=FALSE. Enter "1" to generate output. A visual of this graph can be displayed by calling "fish_plot".
#' @return list with the following elements
#' \describe{
#' \item{Top_Catch}{a list containing the number of fish caught for each location's most-caught fish, in order of location}
#' \item{Top_Species}{a list containing the name of the most-caught species for each location, in order of location}
#' \item{Revenues}{a list containing the total amount of revenue generated for each location, in order of location}
#' \item{rLocations}{a list containing the names of the locations corresponding to the Revenues list}
#' \itme{Fishery_Total}{the total revenue produced by all the locations}
#' \item{Rev_Location_Tot}{a plot of fishery revenue versus location and total revenue}
#' }

# Creating a function that accepts two data tables and a binary entry as inputs
fish_function<- function (fish_counts, prices_df, fish_graph = FALSE) {
  # combining the two data tables into one
  fish_data <- data.frame(fish_counts, price = price_df$prices)
  # creating items for the max count of fish per country
  max_usa = max(usa)
  max_indo = max(indonesia)
  max_phil = max(philippines)
  # creating items for the species names that correspond to the max counts
  dominant_usa = species[which(usa == max_usa)]
  dominant_indo = species[which(indonesia == max_indo)]
  dominant_phil = species[which(philippines == max_phil)]
  # generating country revenues
  # manipulating data into tidy format
  fish_pivot <- pivot_longer(fish_data, c(usa, indonesia, philippines),
                             names_to = "country",
                             values_to = "count")
  # grouping counts by country and adding them together
  frev <- fish_pivot %>% 
    group_by(country) %>% 
    mutate(
      rev = count * prices) %>% 
    summarize(
      total = sum(rev)
    )
  # creating items for each country's counts
  total_usa = frev$total[1]
  total_indo = frev$total[2]
  total_phil = frev$total[3]
  # creating items for the country names that correspond to the counts
  rev_usa = frev$country[which(frev$total == total_usa)]
  rev_indo = frev$country[which(frev$total == total_indo)]
  rev_phil = frev$country[which(frev$total == total_phil)]
  # creating an item for the combined revenue of all countries
  totrev = sum(frev$total)
 
  # generating the outputs for the function (most-caught fish by location with the total caught, total revenue for each location with their totals, the combined total reveue across all locations, and a graph of revenues). And inserting a conditionality statement so that the graph only outputs when requested.
  if(fish_graph == 1){
    # creating a graph of revenue versus each location as well as all combined
    # placing locations and all locations into one vector
    locations <- c(frev$country, "All")
    # placing revenues into one vector
    total_all <- c(frev$total, totrev)
    # creating a dataframe that contains both vectors
    plot_df <- data.frame(locations, total_all)
    # generating the graph
    Rev_Location_Tot <- ggplot(plot_df, aes(x = locations, y = total_all)) +
      geom_col(fill = "green", alpha = 0.8) +
      scale_x_discrete(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(x = "Region",
           y = "Total Fishery Revenue",
           title = "Fishery Revenue by Region") +
      theme_light()
    }
  
  if(fish_graph == 0){
    Rev_Location_Tot <- "No graph"}
                              
return(list(list(Top_Catch = c(max_usa, max_indo, max_phil), Top_Species = c(dominant_usa, dominant_indo, dominant_phil), Revenues = c(total_usa, total_indo, total_phil), rLocations = c(rev_usa, rev_indo, rev_phil), Fishery_Total = totrev, Rev_Location_Tot = fish_graph), Rev_Location_Tot))
}
