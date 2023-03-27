if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("graph")

if (!require("Bioconductor", quietly = TRUE))
  install.packages("Bioconductor")
BiocManager::install("RBGL")

library(tidyverse)
library(corrr)
library(igraph)
library(ggraph)
library(bnlearn)
library(pcalg)
library(miic)








# b. Create the insurance ground truth model from the model string (see insurance help)
#insurance_model <- bn.fit(insurance, method = "hc")

# c. Check the class of the returned object and see the content.
#class(insurance_model)
#insurance_model

# d. Get the adjacency matrix (bnlearn::amat).
#insurance_amat <- amat(insurance_model)
#insurance_amat

# e. Build a directed igraph network from the adjacency matrix and propose a (nice!) plot.
#insurance_graph <- graph.adjacency(insurance_amat, mode = "directed", diag = F)
#plot(insurance_graph, vertex.size = 20, vertex.label = V(insurance_graph)$name)

















data("insurance")

help("insurance")

modelstring = paste0("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]",
                     "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]",
                     "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]",
                     "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]",
                     "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]",
                     "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]",
                     "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]",
                     "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]",
                     "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]",
                     "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]",
                     "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]",
                     "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]")

data = model2network(modelstring)

data

class(data)

mat <- amat(data) #matrice d'adjacence

#Call to hill climbing
myHc <- bnlearn::hc(insurance)  #bnlearn object pas facile à manipuler, passer par un igraph (exploiter la matrice d'adjacence)

#get adj mat from HC graph
myHcAdj <- amat(myHc)

#Create igraph from graph
myIgraph <- igraph::graph_from_adjacency_matrix(amat(data), mode="directed")

plot(myIgraph)

?pc














#2)Score-based method (hill-climbing)

# a. Load the insurance data from the bnlearn package.
data(insurance)

# b. Reconstruct the insurance network using the hill-climbing approach (bnlearn::hc).
insurance_model <- hc(insurance)

# Check the class of the returned object and see the content.
class(insurance_model)
insurance_model

# c. Get the adjacency matrix (bnlearn::amat).
insurance_amat <- amat(insurance_model)
insurance_amat

# Get the adjacency matrix (bnlearn::amat)
true_edges <- amat(insurance_model)

# Get the adjacency matrix of reconstructed model
reconstructed_edges <- as.matrix(insurance_model$arcs)

# d. Build a directed igraph network from the adjacency matrix and propose a (nice!) plot.
insurance_graph <- graph.adjacency(insurance_amat, mode = "directed", diag = F)
plot(insurance_graph, vertex.size = 20, vertex.label = V(insurance_graph)$name)

# e. Count the number of true positive (TP), false positive (FP) and false negative (FN) (for the graph skeleton only). 
# Compute Precision, Recall and Fscore.
#tp <- sum(true_edges & reconstructed_edges)
#fp <- sum(reconstructed_edges & !true_edges)
#fn <- sum(true_edges & !reconstructed_edges)
#tp <- sum(true_edges == 1)
#fp <- sum(true_edges == 0)
#fn <- length(which(true_edges == TRUE & reconstructed_edges == FALSE))
insurance_model_reference <- bn.fit(data, method = "hillclimbing")
cmp_result <- bn.cmp(learn = insurance_model, true.arcs = insurance_model_reference)
tp <- cmp_result$tp
fp <- cmp_result$fp
fn <- cmp_result$fn


#precision <- tp / (tp + fp)
#recall <- tp / (tp + fn)
precision <- ifelse(tp+fp == 0,0,tp / (tp + fp))
recall <- ifelse(tp+fn == 0,0,tp / (tp + fn))
fscore <- 2 * (precision * recall) / (precision + recall)

# f. Highlight the FP edges in your reconstructed network.
fp_edges <- which(!true_edges & reconstructed_edges, arr.ind = TRUE)
E(insurance_graph)[fp_edges[,1] + fp_edges[,2] * nrow(insurance_amat)]$color <- "red"
plot(insurance_graph, vertex.size = 20, vertex.label = V(insurance_graph)$name)

# g. Propose a method to take the orientation into account.
# One possible method is to use the pcalg package to perform PC-Structure algorithm,
# which takes both the graph skeleton and the orientation of the edges into account.
install.packages("pcalg")
library(pcalg)
pcskeleton <- pc(insurance, indepTest = gaussCItest, alpha = 0.01)

