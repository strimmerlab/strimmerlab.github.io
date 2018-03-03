# Reproduces the NCI60 data analysis in Jendoubi and Strimmer. 2018.
# Probabilistic canonical correlation analysis: a whitening approach.
# https://arxiv.org/abs/1802.03490

################ load NCI60 #####################

library("FRCC")  # package needed for NCI60 data

data("Topoisomerase_II_Inhibitors") 
data("microRNA")
X = t(as.matrix(microRNA))
Y = t(as.matrix(Topoisomerase_II_Inhibitors))
dim(X) # 60 365
dim(Y) # 60 15

apply(X, 2, var)
apply(Y, 2, var)


library("CCA")  # package needed for img.matcor() 

# helper function for plotting
corplot = function(X, Y, lambda.cor)
{
  p = dim(X)[2]
  q = dim(Y)[2]

  RXY = (1-lambda.cor)*cor(X, Y)
  Xcor = (1-lambda.cor)*cor(X)+lambda.cor*diag(p)
  Ycor = (1-lambda.cor)*cor(Y)+lambda.cor*diag(q)
  XYcor =  rbind( cbind(Xcor, RXY), cbind(t(RXY), Ycor))
  correl = list(Xcor=Xcor, Ycor=Ycor, XYcor=XYcor)

  img.matcor(correl, type = 2) # from CCA package
}


################ start CCA analysis #######################

# load "whitening" library
library("whitening")

cca.out = scca(X, Y, scale=TRUE)
cca.out

# shrinkage intensity
lambda.cor = cca.out$lambda.cor
lambda.cor # 0.2471984

# estimated canonical correlations
cca.out$lambda
# [1]  0.9362432  0.8583791  0.7741422  0.7306245  0.6834722  0.6122642
# [7]  0.5588427  0.5226476 -0.4863341 -0.4177760  0.3971996  0.3520358
# [13]  0.3260580 -0.2979417  0.2359118

sum(cca.out$lambda < 0) # 3

range(cca.out$lambda) # -0.4863341  0.9362432

# plot canonical correlations
#pdf(file="nci60-barplot.pdf", width=7, height=5)
    barplot(cca.out$lambda, xlab = "Dimension", 
    ylab = "Canonical correlations", names.arg = 1:length(cca.out$lambda))
#dev.off()

# plot correlations
#pdf(file="nci60-matcor.pdf", width=7, height=7)
    corplot(X,Y, cca.out$lambda.cor)
#dev.off()


