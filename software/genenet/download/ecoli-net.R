#######################################################################
# This note can be directly run in R.
# Requires GeneNet 1.2.7 (June 2013)
#######################################################################


# This reproduces the "Ecoli" network example is from:

# Schaefer, J., and K. Strimmer, K. 2005c.  A shrinkage approach
# to large-scale covariance estimation and implications for
# functional genomics.   Statist. Appl.  Genet. Mol. Biol. 4: 32
# http://www.bepress.com/sagmb/vol4/iss1/art32/


# load GeneNet library
library("GeneNet")

# Example E. Coli data set (102 genes, 9 time points)
data(ecoli)
dim(ecoli)

########################################################################
###
### Step 1: Estimate partial correlation matrix
###

# there are many methods for estimating partial correlations
# - we recommend using a shrinkage estimator 

pc <- ggm.estimate.pcor(ecoli)
dim(pc)

########################################################################
###
### Step 2: Assign p-values, q-values, empirical posterior probabilities 
###         to all potential edges in the network
###

ecoli.edges <- network.test.edges(pc, direct=TRUE, fdr=TRUE)
dim(ecoli.edges) # 5151   10
ecoli.edges[1:4,]

########################################################################
###
### Step 3: Decide which edges to include in the network
###

# use local fdr cutoff 0.2
ecoli.net <- extract.network(ecoli.edges)
dim(ecoli.net)

# use local fdr cutoff 0.1
ecoli.net <- extract.network(ecoli.edges, cutoff.ggm=0.9, cutoff.dir=0.9)
dim(ecoli.net)

# take the 70 most significant edges
ecoli.net <-extract.network(ecoli.edges, method.ggm="number", cutoff.ggm=70)

########################################################################
###
### Step 4: Plot the network
###

node.labels <- colnames(ecoli)


##
## variant 1: use "igraph" R package for plotting
##            (seehttp://igraph.sourceforge.net/ )

library("igraph") # is automatically loaded with GeneNet 1.2.7

# produce graph (*without* edge labels)
igr1 <- network.make.igraph(ecoli.net, node.labels)
plot(igr1, main="Ecoli Network")

# there are many options available in igraph to fine-tune this plot.
# e.g. smaller node labels and arrow sizes:
plot(igr1, main="Ecoli Network", vertex.label.cex=0.7, edge.arrow.size=0.5)

# produce graph (*with* edge labels)
igr2 <- network.make.igraph(ecoli.net, node.labels, show.edge.labels=TRUE)
plot(igr2, main="Ecoli Network with Partial Correlations")


##
## variant 2: produce "dot" file for use with "graphviz"
##            (see http://www.graphviz.org/ )


# produce dot file (*without* edge labels)
network.make.dot("ecoli.dot", ecoli.net, node.labels,
                  main="Ecoli Network")

# call graphviz to produce a nice graph
system("fdp -T svg -o ecoli.svg ecoli.dot") # SVG format
system("fdp -T ps2 -o ecoli.eps ecoli.dot") # EPS format
system("fdp -T png -o ecoli.png ecoli.dot") # PNG format


# produce dot file (*with* edge labels)
network.make.dot("ecoli.dot", ecoli.net, node.labels,
                  show.edge.labels=TRUE, main="Ecoli Network")

# call graphviz to produce a nice graph
system("fdp -T svg -o ecoli.svg ecoli.dot") # SVG format
system("fdp -T ps2 -o ecoli.eps ecoli.dot") # EPS format
system("fdp -T png -o ecoli.png ecoli.dot") # PNG format


# Hint: this requires the proper installation of graphviz
#       so that its associated layout programs such as 
#       neato, fdp etc. can be called on the command line
# if the system() call doesn't work for you (e.g. on Windows) 
# simply use a GUI frontend for graphviz and 
# process the dot file from there.


