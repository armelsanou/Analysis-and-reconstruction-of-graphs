# We consider a 'random' network with 10000 vertices, and a connection probability of 1e-3
n_nodes = 10000
prob_connection = 1e-2
# degree distribution
# NB: for each node, we ask whether it is connected to all other nodes
rbinom(n_nodes, n_nodes, prob = prob_connection)
hist(c(0, rbinom(n_nodes, n_nodes, prob = prob_connection)))
# ==> no high degree!! even with 10000 nodes

# ----------------
# RANDOM NETWORK
# ----------------

# Erdos-Renyi model
n_nodes = 10000
prob_connection = 1e-2
g_erdos.renyi = erdos.renyi.game(n_nodes, prob_connection)
hist(igraph::degree(g_erdos.renyi, mode = "all"))
max(igraph::degree(g_erdos.renyi, mode = "all"))

# Albert-Barbasi model
?barabasi.game
n_nodes = 10000
#prob_connection = 1e-3
g_barabasi = barabasi.game(n_nodes, m = 2)
hist(igraph::degree(g_barabasi, mode = "all"), 
     breaks = "FD")
max(igraph::degree(g_barabasi, mode = "all"))

# ----------------
# Degree distribution log-log plot
# ----------------
# Visualize the degree distribution
plot_degree_distribution <- function(graph, is.plot = TRUE){
  d = igraph::degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  nonzero_position = which(probability != 0)
  probability = probability[nonzero_position]
  degree = degree[nonzero_position]
  if(is.plot){
    plot(probability~degree, log = "xy",
         xlab = "Degree (log scale)", ylab = "Probability (log scale)",
         col = 1, main = "Deegree Distribution")
  }
  
  ret_list = list(proba = probability, deg = degree)
  return(ret_list)
}
plot_degree_distribution(g_erdos.renyi)
plot_degree_distribution(g_barabasi)

# plot and fit the power law distribution
fit_power_law = function(graph) {
  # calculate degree
  d = igraph::degree(graph, mode = "all")
  dd = degree.distribution(graph, mode = "all", cumulative = FALSE)
  degree = 1:max(d)
  probability = dd[-1]
  # delete blank values
  nonzero.position = which(probability != 0)
  probability = probability[nonzero.position]
  degree = degree[nonzero.position]
  reg = lm(log(probability) ~ log(degree))
  cozf = coef(reg)
  power.law.fit = function(x) exp(cozf[[1]] + cozf[[2]] * log(x))
  alpha = -cozf[[2]]
  R.square = summary(reg)$r.squared
  print(paste("Alpha =", round(alpha, 3)))
  print(paste("R square =", round(R.square, 3)))
  # plot
  plot(probability ~ degree, log = "xy", xlab = "Degree (log)", ylab = "Probability (log)", 
       col = 1, main = "Degree Distribution")
  curve(power.law.fit, col = "red", add = T, n = length(d))
}
fit_power_law(g_erdos.renyi)
fit_power_law(g_barabasi)

# ----------------
# Clustering coefficient
# ----------------
library(igraph)
adj <- rbind(
  c(0,1,0,0,1,0),
  c(0,0,1,0,1,0),
  c(0,0,0,0,0,0),
  c(0,0,1,0,1,1),
  c(0,0,0,0,0,0),
  c(0,0,0,0,0,0))
adj

g = graph_from_adjacency_matrix(adj, 
            mode = "directed", 
            weighted = T)
plot(g)

# Global CC
# All closed triplets (x3!) on all open triplets
# 3(1) / 11
cc_global = transitivity(g, type="global")
cc_global

# Local CC
V(g)
transitivity(g, vids = 5, type ="local")

# ----------------
# Diameter
# ----------------
test.graph = barabasi.game(100, power = 0.9, m=2)
plot(test.graph, vertex.size = 7, vertex.label.cex = .5, 
     edge.arrow.size = .5)

# Compute the diameter and get the nodes
diameter(test.graph)
nodes.diameter <- get.diameter(test.graph)

# See the diameter
V(test.graph)$color <- "skyblue"
V(test.graph)$size <- 7
V(test.graph)[nodes.diameter]$color <- "darkgreen"
V(test.graph)[nodes.diameter]$size <- 10
V(test.graph)[nodes.diameter]$label.color <- "white"

E(test.graph)$color <- "grey"
E(test.graph)$width <- 1
E(test.graph, path = nodes.diameter)$color <- "darkgreen"
E(test.graph, path = nodes.diameter)$width <- 2

plot(test.graph, layout =layout.fruchterman.reingold,
     vertex.label.cex = .5, edge.arrow.size = .5)

