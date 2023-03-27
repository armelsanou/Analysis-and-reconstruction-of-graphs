library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)

# First example: mtcars
# --------

# Create a tidy data frame of correlations, format required for ggraph
# see: https://juba.github.io/tidyverse/12-tidyr.html
# Three rules to be 'tidy'
# --> each line is an observation
# --> each column is a variable
# --> each value is in a unique cell of the table
head(mtcars)
help(mtcars)

tidy_cors <- mtcars %>% 
              correlate() %>% 
              stretch()
head(tidy_cors, n = 20)

# Remove 'duplicated'
tidy_cors[which(tidy_cors$x == "cyl" & tidy_cors$y == "wt"),]
tidy_cors[which(tidy_cors$x == "wt" & tidy_cors$y == "cyl"),]

tidy_cors_edgeNames <- apply(tidy_cors[, c("x", "y")], 1, function(x) paste(sort(x), collapse = "--"))
tidy_cors = tidy_cors[!duplicated(tidy_cors_edgeNames),]

tidy_cors[which(tidy_cors$x == "cyl" & tidy_cors$y == "wt"),]
tidy_cors[which(tidy_cors$x == "wt" & tidy_cors$y == "cyl"),]

# Convert the data table into an undirected graph (correlation is not directed!)
# NB: keep only correlation with abs > 0.7
graph_cors <- tidy_cors %>%
              filter(abs(r) > .7) %>%
              graph_from_data_frame(directed = FALSE)
graph_cors

# Plot with ggraph
ggraph(graph_cors) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name))

# Make a fancy plot
# 1 --> improve edges display
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  geom_node_point() +
  geom_node_text(aes(label = name))

# 2 --> reduce side legend
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  geom_node_point() +
  geom_node_text(aes(label = name))

# 3 --> Improve the color gradient
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2"))+
  geom_node_point() +
  geom_node_text(aes(label = name))

# 4 --> Improve node display and associated text
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2"))+
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE)

# 5 --> Remove background grid and set title
ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2"))+
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between cars variables (abs min = 0.7)")

# Does the relationships make sense1?

# Check some partial correlations:
# Triplet: wt - mpg - cyl
res_wt_mpg = lm(mtcars$wt ~ mtcars$mpg)$residuals
res_cyl_mpg = lm(mtcars$cyl ~ mtcars$mpg)$residuals
cor(mtcars$wt, mtcars$cyl)
# [1] 0.7824958

cor(res_wt_mpg, res_cyl_mpg)
# [1] 0.1657085

# Modifiy the correlation and plot the graph
tidy_cors[which(tidy_cors$x == "cyl" & tidy_cors$y == "wt"),]$r = cor(res_wt_mpg, res_cyl_mpg)
tidy_cors[which(tidy_cors$x == "wt" & tidy_cors$y == "cyl"),]$r = tidy_cors[which(tidy_cors$x == "cyl" & tidy_cors$y == "wt"),]$r

graph_cors <- tidy_cors %>%
  filter(abs(r) > .7) %>%
  graph_from_data_frame(directed = FALSE)

ggraph(graph_cors) +
  geom_edge_link(aes(edge_alpha = abs(r), edge_width = abs(r), color = r)) +
  guides(edge_alpha = "none", edge_width = "none") +
  scale_edge_colour_gradientn(limits = c(-1, 1), colors = c("firebrick2", "dodgerblue2"))+
  geom_node_point(color = "white", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_graph() +
  labs(title = "Correlations between cars variables (abs min = 0.7; pcor wt-cyl)")
