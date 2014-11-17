# /*
# This is an R script containing R markdown comments.  It can be run as is in R.
# To generate a document containing the formatted R code, R output and markdown 
# click the "Compile Notebook" button in R Studio, or run the command
# rmarkdown::render() - see http://rmarkdown.rstudio.com/r_notebook_format.html
# */

#' ---
#' title: "Arabidopsis Thaliana Network"
#' output: pdf_document
#' author: ""
#' date: Example for GeneNet 1.2.7 (June 2013) or later
#' ---

#' This note reproduces the “Arabidopsis thaliana” network example from
#' R. Opgen-Rhein and K. Strimmer. 2007. *From correlation to causation 
#' networks: a simple approximate learning algorithm and its application 
#' to high-dimensional plant gene expression data.*
#' BMC Syst. Biol. **1**: 37.
#' (http://dx.doi.org/10.1186/1752-0509-1-37)

#'  
#'The original source of the data is 
#' Smith et al. 2004. *Diurnal changes in the transcriptom encoding 
#' enzymes of starch metabolism provide evidence for both transcriptional
#' and posttranscriptional regulation of starch metabolism in Arabidopsis 
#' leaves.*  Plant Physiol. **136**: 2687-2699.
#'  

#' This example was suggested by
#' Papapit Ingkasuwan, Division of Biotechnology,
#' School of Bioresources and Technology,
#' King Mongkut's University of Technology Thonburi, Bangkok, Thailand.


#'
#' # Inspect Data

library("GeneNet")
data("arth800")
summary(arth800.expr)

#' Plot time series:
plot(arth800.expr, 1:9)

#' Inspect pairwise scatter plots:
panel.cor = function(x, y, digits=2, prefix="", cex.cor)
{
    usr = par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r = abs(cor(x, y))
    txt = format(c(r, 0.123456789), digits=digits)[1]
    txt = paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex = 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}
#+ fig.width=8, fig.height=10
pairs(arth800.expr[,1:9], lower.panel=panel.smooth, upper.panel=panel.cor)


#'
#' # Compute Partial correlations and Construct Graph

pcor.dyn = ggm.estimate.pcor(arth800.expr, method = "dynamic")
arth.edges = network.test.edges(pcor.dyn,direct=TRUE)
dim(arth.edges)

#' We use the strongest 150 edges:
arth.net = extract.network(arth.edges, method.ggm="number", cutoff.ggm=150)


#'
#' # Plot Network

node.labels = as.character(1:ncol(arth800.expr))
igr = network.make.igraph(arth.net, node.labels)

#+ fig.width=8, fig.height=10
plot(igr, main="Arabdiopsis Network", layout=layout.fruchterman.reingold,
 edge.arrow.size=0.5, vertex.size=9, vertex.label.cex=0.7)


#'
#' # Further Analysis

#' Some of the discovered hubs:
sort(degree(igr), decreasing=TRUE)[1:10]
# /*
# 570  81 783  47 422 558 452 539 738 272
# 20  17  10   9   9   9   8   8   8   7
# */

arth800.descr[570]
# /* [1] "AP2 transcription factor - like protein" */

arth800.descr[81]
# /* [1] "ATRPAC43; DNA binding / DNA-directed RNA polymerase;  */

arth800.descr[558]
# /* [1]"structural constituent of ribosome; */

arth800.descr[539]
# /* [1] "DNA binding / transcription factor;  */

arth800.descr[783]
# /* [1] "RNA binding / RNA methyltransferase; */


