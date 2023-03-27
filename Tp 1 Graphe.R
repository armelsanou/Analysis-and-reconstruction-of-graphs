library("miic")
library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)

hematoData
dim(hematoData)

num_data <- sapply( hematoData, as.numeric )

tidy_cors <- num_data %>% 
  correlate() %>% 
  stretch()

graph_cors <- tidy_cors %>%
  filter(abs(r) > .3) %>%
  graph_from_data_frame(directed = FALSE)


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
  labs(title = "Correlations between hematoData variables (abs min = 0.3)")

