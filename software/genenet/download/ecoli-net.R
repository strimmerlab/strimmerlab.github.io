# /*
# This is an R script containing R markdown comments.  It can be run as is in R.
# To generate a document containing the formatted R code, R output and markdown 
# click the "Compile Notebook" button in R Studio, or run the command
# rmarkdown::render() - see http://rmarkdown.rstudio.com/r_notebook_format.html
# */


#' ---
#' title: "Escherichia Coli Network"
#' output: pdf_document
#' author: ""
#' date: Example for GeneNet 1.2.7 (June 2013) or later
#' ---

#' This note reproduces the “Escherichia coli” network example from J. Schäfer and 
#' K. Strimmer. 2005. *A shrinkage approach to large-scale covariance 
#' estimation and implications for functional genomics.* 
#' Statist. Appl. Genet. Mol. Biol. **4**: 32 
#' (http://dx.doi.org/10.2202/1544-6115.1175)


#'
#' # Load GeneNet package

library("GeneNet")

#' E. Coli data set (9 time points for 102 genes):
data(ecoli)
dim(ecoli)


#'
#' # Estimation of partial correlations

#' Estimate matrix of partial correlation using a shrinkage estimator:
pc = ggm.estimate.pcor(ecoli)
dim(pc)

#' Assign p-values, q-values and empirical posterior probabilities to all
#' 5151 potential edges in the network:
ecoli.edges = network.test.edges(pc, direct=TRUE, fdr=TRUE)
dim(ecoli.edges)

#' The table lists all edges in the order strength of partial correlations:
ecoli.edges[1:5,]

#'
#' # Decide which edges to include in the network

#' To obtain a graph you need to select top ranking edges according to 
#' a suitable criterion.  Here are some suggestions:
#'
#' 1. Use local fdr cutoff 0.2, i.e. include all edges with posterior 
#' probability of at least 0.8.
ecoli.net = extract.network(ecoli.edges)
dim(ecoli.net)

#' 2. Use local fdr cutoff 0.1, i.e. i.e. include all edges with posterior 
#' probability of at least 0.9.

ecoli.net = extract.network(ecoli.edges, cutoff.ggm=0.9, cutoff.dir=0.9)
dim(ecoli.net)

#' 3. Include a fixed number of edges, say the 70 strongest edges
ecoli.net = extract.network(ecoli.edges, method.ggm="number", cutoff.ggm=70)
dim(ecoli.net)

#' 
#' Plot network

#' For plotting we use the igraph package (http://igraph.org).
#' Note igraph is automatically installed with GeneNet.
library("igraph") # 

#' Create igraph object from the list of edges:
node.labels = colnames(ecoli)
igr1 = network.make.igraph(ecoli.net, node.labels)
igr1

#' Plot the network:
#+ fig.width=8, fig.height=8
plot(igr1, main="Ecoli Network", vertex.label.cex=0.8, edge.arrow.size=0.8)

#' It is also possible to produce a graph showing correlations as edge labels:
igr2 = network.make.igraph(ecoli.net, node.labels, show.edge.labels=TRUE)
#+ fig.width=8, fig.height=8
plot(igr2, main="Ecoli Network with Partial Correlations as Edge Labels",
vertex.label.cex=0.8, edge.arrow.size=0.8)


