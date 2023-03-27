# 'mtcars' dataset
# ----
library(ggraph)
library(igraph)
library(Hmisc)
library(ppcor)

# See mtcars (we will consider only the first 7 columns)
?mtcars; head(mtcars)

# CORRELATION GRAPH
# --------

# Compute the pairwise correlation coefficients
# and their significativity
# --
my_mtcars <- scale(mtcars[, 1:7])
head(my_mtcars)
res_corr <- rcorr(my_mtcars, type="pearson")
res_corr

# We get 2 matrices
# $r : pearson correlation or spearman rho
# $P: correlation p-values
graph_corrs <- graph_from_adjacency_matrix(res_corr$r, 
                                           diag = FALSE, 
                                           weight = TRUE, 
                                           mode = "undirected")
plot(graph_corrs, layout=layout_with_dh)

# Filter correlations from correlations (keep > 0.3 in abs)
thresh = 0.6
res_corr_filter_corr <- res_corr$r
res_corr_filter_corr[abs(res_corr$r) < thresh] <- 0
graph_corrs_filter_corr <- graph_from_adjacency_matrix(res_corr_filter_corr, 
                                                       diag = FALSE, 
                                                       weight = TRUE, 
                                                       mode = "undirected")

plot(graph_corrs_filter_corr, layout=layout_with_dh)

# Use correlation for edge width and colors
plot(graph_corrs_filter_corr, 
     layout=layout_with_dh, 
     edge.width = abs(E(graph_corrs_filter_corr)$weight),
     edge.color = ifelse(E(graph_corrs_filter_corr)$weight > 0, 
                         "blue","red"))

# PARTIAL CORRELATION GRAPH
# --------
res_corr_part <- ppcor::pcor(my_mtcars)
res_corr_part <- res_corr_part$estimate

# Filter correlations from correlations (keep > 0.3 in abs)
thresh = 0.2
res_corr_filter_corr <- res_corr_part
res_corr_filter_corr[abs(res_corr_part) < thresh] <- 0
graph_corrs_filter_corr_part <- graph_from_adjacency_matrix(res_corr_filter_corr, diag = FALSE, weight = TRUE, mode = "undirected")

# Use correlation for edge width and colors
plot(graph_corrs_filter_corr_part, layout=layout_in_circle, 
     edge.width = abs(E(graph_corrs_filter_corr_part)$weight),
     edge.color = ifelse(E(graph_corrs_filter_corr_part)$weight > 0, "blue","red"))

# DIFF
# --------
E(graph_corrs_filter_corr)
E(graph_corrs_filter_corr_part)

# Edge cyl--wt disappear... conditioning on what?
# --------
# Correlation
cor(mtcars$cyl, mtcars$wt)

# Influence of drat?
mm1 = lm(mtcars$cyl ~ mtcars$drat)
mm2 = lm(mtcars$wt ~ mtcars$drat)
cor(mm1$residuals, mm2$residuals)

# Influence of mpg?
mm1 = lm(mtcars$cyl ~ mtcars$mpg)
mm2 = lm(mtcars$wt ~ mtcars$mpg)
cor(mm1$residuals, mm2$residuals)

# Influence of disp?
mm1 = lm(mtcars$cyl ~ mtcars$disp)
mm2 = lm(mtcars$wt ~ mtcars$disp)
cor(mm1$residuals, mm2$residuals)

# Combined influence?
library(ppcor)
pcor(my_mtcars[,c("cyl", "wt")])

# ... conditioning on disp, drat and mpg
# ------------------
par_cor_vs_all = pcor(my_mtcars[,c("cyl", "wt", "drat", "mpg", "disp")])

# ... conditioning on disp, drat and mpg
# ------------------
par_cor_vs_all = pcor(my_mtcars[,c("cyl", "wt", "drat", "mpg")])

