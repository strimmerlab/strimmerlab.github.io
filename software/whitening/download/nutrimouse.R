
# Reproduces the nutrimouse data analysis in Jendoubi and Strimmer. 2018.
# Probabilistic canonical correlation analysis: a whitening approach.
# https://arxiv.org/abs/1802.03490

################ load nutrimouse data #####################

library("CCA")  # package needed for nutrimouse data and img.matcor() function

data("nutrimouse")
X = as.matrix(nutrimouse$gene)
Y = as.matrix(nutrimouse$lipid)
dim(X) # 40 120
dim(Y) # 40 21

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

# CCA with shrinkage estimate of joint correlation
cca.out = scca(X, Y, scale=TRUE)
cca.out

# shrinkage intensity
lambda.cor = cca.out$lambda.cor
lambda.cor # 0.1599767

# estimated canonical correlations
cca.out$lambda
# [1] -0.9605275617 -0.9476499457 -0.9264858941  0.8660390307 -0.8591510518
# [6] -0.8057844616 -0.7314092050 -0.7192164996 -0.6702938353 -0.6416074974
#[11] -0.4937654758  0.4686423030  0.3895309296 -0.3588989722 -0.3049187235
#[16] -0.2546627651 -0.2261556012  0.1596697930 -0.1179019917 -0.1013979520
#[21]  0.0009561428

sum(cca.out$lambda < 0) # 16

range(cca.out$lambda) # -0.9605276  0.8660390

# plot canonical correlations
#pdf(file="nutrimouse-barplot.pdf", width=7, height=5)
    barplot(cca.out$lambda, xlab = "Dimension", 
    ylab = "Canonical correlations", names.arg = 1:length(cca.out$lambda))
#dev.off()

# plot correlations
#pdf(file="nutrimouse-matcor.pdf", width=7, height=7)
    corplot(X,Y, cca.out$lambda.cor)
#dev.off()
