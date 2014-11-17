# /*
# This is an R script containing R markdown comments.  It can be run as is in R.
# To generate a document containing the formatted R code, R output and markdown 
# click the "Compile Notebook" button in R Studio, or run the command
# rmarkdown::render() - see http://rmarkdown.rstudio.com/r_notebook_format.html
# */


#' ---
#' title: "T-Cell Network"
#' output: pdf_document
#' author: ""
#' date: Example for GeneNet 1.2.7 (June 2013) or later
#' ---

#' This note reproduces the “T-Cell” network example from 
#' R. Opgen-Rhein and K. Strimmer. 2006a.  *Using regularized dynamic
#' correlation to infer gene dependency networks from time-series 
#' microarray data.*  Proceedings of WCSB 2006 (June 12-13, 2006, 
#' Tampere, Finland) and R. Opgen-Rhein  and K. Strimmer. 2006b. 
#' *Inferring gene dependency networks from genomic longitudinal data: 
#' a functional data approach.* REVSTAT **4**:53-65. 
#' (http://www.ine.pt/revstat/pdf/rs060103.pdf)


#'
#' # Load GeneNet library
library("GeneNet")

#' get T cell data 
data(tcell)
tc44 = combine.longitudinal(tcell.10, tcell.34)

#'
#' # Estimate partial correlations

pc1 = ggm.estimate.pcor(tc44, lambda=0)                   # static, no shrinkage
pc2 = ggm.estimate.pcor(tc44, method="dynamic", lambda=0) # dynamic, no shrinkage
pc3 = ggm.estimate.pcor(tc44)                             # static, with shrinkage         
pc4 = ggm.estimate.pcor(tc44, method="dynamic")           # dynamic, with shrinkage


#'
#' # Find significant edges

#' We use as selection criterion local fdr <= 0.2
#'  

#' static, no shrinkage
t1.edges = ggm.test.edges(pc1)
t1.net = extract.network(t1.edges) # prob > 0.8
t1.net

#' dynamic, no shrinkage
t2.edges = ggm.test.edges(pc2)
t2.net = extract.network(t2.edges) # prob > 0.8
t2.net

#' static, with shrinkage
t3.edges = ggm.test.edges(pc3)
t3.net = extract.network(t3.edges) # prob > 0.8
t3.net

#' dynamic, with shrinkage
t4.edges = ggm.test.edges(pc4)
t4.net = extract.network(t4.edges) # prob > 0.8
t4.net


#'
#' # Produce plots using igraph

node.labels = colnames(tc44)
igr1 = ggm.make.igraph( t1.net, node.labels) 
igr2 = ggm.make.igraph( t2.net, node.labels)  
igr3 = ggm.make.igraph( t3.net, node.labels) 
igr4 = ggm.make.igraph( t4.net, node.labels)  

#+ fig.width=8, fig.height=8
par(mfrow=c(2,2))
plot(igr1, main="Static, no shrinkage", vertex.label.cex=.6, vertex.size=25)
plot(igr2, main="Dynamic, no shrinkage", vertex.label.cex=.6, vertex.size=25)
plot(igr3, main="Static, with shrinkage", vertex.label.cex=.6, vertex.size=25)
plot(igr4, main="Dynamic, with shrinkage", vertex.label.cex=.6, vertex.size=25)
par(mfrow=c(1,1))


