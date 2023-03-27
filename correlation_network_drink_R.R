rm(list=ls(all=TRUE))

library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)

library(fivethirtyeight)
library(countrycode)

# Seconde example: fivethirtyeight --> drinks
# --------

# Create a tidy data frame of correlations, format required for ggraph
# see: https://juba.github.io/tidyverse/12-tidyr.html
# Three rules to be 'tidy'
# --> each line is an observation
# --> each column is a variable
# --> each value is in a unique cell of the table
head(drinks)
dim(drinks)
help(drinks)

drinks = drinks[-which(drinks$country == "Micronesia"),]

# Objective: Find which countries in Europe and the US have similar patterns of beer,
# wine and spirit drinking. We use the 'countrycode' package to bind continent information.
# We are interested in particular to Autralia
d <- drinks %>% 
      mutate(continent = countrycode(country, "country.name", "continent")) %>% 
      filter(continent %in% c("Europe", "Americas") | country == "Australia") %>%
      select(country, contains("servings"))

head(d)
dim(d)

#min(d$beer_servings)
#max(d$beer_servings)
#min(d$spirit_servings)
#max(d$spirit_servings)

# Scale data to observe relative amounts rather than absolute volume of beer/wine/spirits
scaled_data <- d %>%
                mutate_if(is.numeric, scale)

#head(scaled_data)
#dim(scaled_data)
#min(scaled_data$beer_servings)
#max(scaled_data$beer_servings)
#min(scaled_data$spirit_servings)
#max(scaled_data$spirit_servings)

# Create a tidy data frame of correlations, format required for ggraph
# see: https://juba.github.io/tidyverse/12-tidyr.html
# Three rules to be 'tidy'
# --> each line is an observation
# --> each column is a variable
# --> each value is in a unique cell of the table
tidy_data <- scaled_data %>%
              gather(type, litres, -country) %>%
              drop_na() %>%
              group_by(country) %>%
              filter(sd(litres) > 0) %>%
              ungroup()
head(tidy_data)

# Widen into suitable format for correlations
wide_data <- tidy_data %>%
              spread(country, litres) %>%
              select(-type)
wide_data

tidy_cors <- wide_data %>% 
  correlate() %>% 
  stretch()
head(tidy_cors)

# Convert the data table into an undirected graph (correlation is not directed!)
# NB: keep only correlation with abs > 0.3
graph_cors <- tidy_cors %>%
              filter(r > .9) %>%
              graph_from_data_frame(directed = FALSE)
graph_cors

# Plot with ggraph
ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

# Make a fancy plot
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2"))+
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between countries based on drinks")

# Does the relationships make sense?
