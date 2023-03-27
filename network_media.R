rm(list=ls(all=T))

library(igraph)

# ----------------
# EXAMPLE 1:
# ----------------

# Load the datasets:
# - Media_Examples_NODES.csv
# - Media_Examples_EDGES.csv
setwd("~/Documents/Enseignement/Paris5/AnalyseEtReconstructionDeReseaux/data")
nodes <- read.csv("Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
links <- read.csv("Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)

# Vizualize the first rows
# Get the number of rows
# Get the number of links
head(nodes, n=20)
head(links, n=20)

nrow(nodes)
length(unique(nodes$id))

nrow(links)
nrow(unique(links[,c("from", "to")]))

# Aggregate redundant links and order
help("aggregate")
# aggregate(x, by, FUN, ..., simplify = TRUE, drop = TRUE)
# arg1: col weight, arg2: table without weight, arg3: sum 
links <- aggregate(links[, 3], links[, -3], sum)
head(links)
links <- links[order(links$from, links$to),]
head(links)
colnames(links)[4] <- "weight"
rownames(links) <- NULL
head(links, n=25)
dim(links)

# Create the graph
net <- igraph::graph_from_data_frame(d = links, vertices = nodes, directed = "T")
net; plot(net)

# D or U: directed or undirected
# N: names (attribute 'name' for nodes)
# W: weigthed (attribute 'weight' for links)
# (v/c): vertex-level character attribute
# (e/n): edge-level numeric attribute

# Access the vertices
V(net)
# Access the edges
E(net)
# Access the "type" attribute of edges
E(net)$type
# Access the "media" attribute of vertices
V(net)$media

# Find vertices or edges by attribute
V(net)[media == "BBC"]
E(net)[type == "mention"]


# Look directly into the adjacency matrix
net[1:10,]
net[1,]
net[5,7]

# Get the list of edges or nodes
as_edgelist(net, names = TRUE)
as_edgelist(net, names = FALSE)

# Get the adjacency matrix
as_adjacency_matrix(net, attr = "weight")

# Make a simple plot
# NB: notice loops!
plot(net)

# Remove loops
?simplify

net <- igraph::simplify(net, remove.loops = TRUE) 
plot(net)

# Lots of parameters with igraph
# Vertices:
# vertex.color, vertex.frame.color, vertex.shape, vertex.size, vertex.label,
# vertex.label.font, vertex.label.cex, vertex.label.dist, vertex.label.degree
# Edges:
# edge.color, edge.width, edge.arrow.size, edge.arrow.width, edge.lty, edge.label,
# edge.label.family, edge.label.font, edge.label.cex, edge.curved

# Fancy plot
plot(net, edge.arrow.size = .4, edge.curved = 1)
plot(net, edge.arrow.size = .2, 
     edge.color = "orange", vertex.color = "orange",
     vertex.frame.color ="#ffffff", vertex.label = V(net)$media,
     vertex.label.color = "black")


V(net)$name = V(net)$media
plot(net, edge.arrow.size = .4, edge.curved = 1)
plot(net, edge.arrow.size = .2, 
     edge.color = "orange", vertex.color = "orange",
     vertex.frame.color ="#ffffff", 
     vertex.label.color = "black")

net.df <- igraph::as_data_frame(net)
all_v = sort(unique(c(net.df$from, net.df$to)))
net.adj <- matrix(0, length(all_v), length(all_v))
colnames(net.adj) = rownames(net.adj) = all_v

for(iEdge in 1:nrow(net.df)){
  net.adj[net.df[iEdge, "from"], net.df[iEdge, "to"]] <- net.df[iEdge, "weight"]
  net.adj[net.df[iEdge, "to"], net.df[iEdge, "from"]] <- net.df[iEdge, "weight"]
}
head(net.adj)
colnames(net.adj) = rownames(net.adj) = V(net)$media

# DEGREE / DEGREE DISTRIBUTION
# ----------------
# Get the degrees (in and out, in, out)
igraph::degree(net, mode = "all")
igraph::degree(net, mode = "in")
igraph::degree(net, mode = "out")

# Get the number of nodes for all possible degrees [0, max]
max_deg = max(igraph::degree(net, mode = "all"))
min_deg = 0
all_deg = rep(min_deg, max_deg+1)
names(all_deg) = seq(from = 0, to = max_deg)
all_deg_count = table(igraph::degree(net, mode = "all"))
all_deg[as.numeric(names(all_deg_count))+1] = all_deg_count
# --> all_deg contains the number of node for each possible degree
all_deg_pk = all_deg/length(unique(nodes$id))
all_deg_pk

# Compare to the degree_distribution function
help(degree_distribution)
degree_distribution(net, cumulative = FALSE, mode = "all")

# Average degree
mean_deg = sum(as.numeric(names(all_deg_pk))[-1]*all_deg_pk[-1])
mean_deg

# Plot the cumulative degree distribution
deg_dist <- degree_distribution(net, cumulative = TRUE, mode = "all")
plot(x = min_deg:max_deg, y = (1 - deg_dist),
     pch = 19, cex = 1.2, col = "orange",
     xlab = "Degree", ylab = "Cumulative Frequency")

# IMPROVE GRAPH DISPLAY WITH NODE DEGREE (and other features...)
# ----------------
# Generate colors based on media type
colors <- c("gray50", "tomato", "gold")
V(net)$color <- colors[V(net)$media.type]
plot(net)

# Compute node degrees and use it for node size
deg <- igraph::degree(net, mode = "all")
V(net)$size <- deg*2.5
plot(net)

# Erase default node label
V(net)$label <- NA
plot(net)

# Set edge width based on 'weight'
E(net)$width <- E(net)$weight/6
plot(net)

# Change arrow size and edge color
E(net)$arrow.size <- .4
E(net)$edge.color <- "gray80"
E(net)$width <- 1+E(net)$weight/12
plot(net)

# Add a legend
legend(x = -1.5, y = 1.1, c("Newspaper", "Television", "Online News"), 
       pch = 21, col = "#777777", pt.bg = colors, 
       pt.cex = 1.8, cex = .9, bty = "n", ncol = 1)

# Finally
plot(net, vertex.label = V(net)$media)
legend(x = -1.5, y = 1.1, c("Newspaper", "Television", "Online News"), 
       pch = 21, col = "#777777", pt.bg = colors, 
       pt.cex = 1.8, cex = .8, bty = "n", ncol = 1)

# Add color edges based on their source node
edge.start <- ends(net, es = E(net), names = F)[,1]
edge.col <- V(net)$color[edge.start]
plot(net, edge.color = edge.col, vertex.label = V(net)$media)
legend(x = -1.5, y = 1.1, c("Newspaper", "Television", "Online News"), 
       pch = 21, col = "#777777", pt.bg = colors, 
       pt.cex = 1.8, cex = .8, bty = "n", ncol = 1)

# If semantic network, plot only labels
plot(net, edge.color = edge.col, vertex.label = V(net)$media,
     vertex.shape = "none")

