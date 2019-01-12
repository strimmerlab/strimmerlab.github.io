# Reproduces the nutrimouse data analysis in
# Jendoubi and Strimmer. 2019. A whitening approach to probabilistic canonical 
# correlation analysis for omics data integration.  BMC Bioinformatics 20:15.
# https://doi.org/10.1186/s12859-018-2572-9


##### load package and data ################################

# load "whitening" library
library("whitening") # contains CCA functions and data

# load data
data(nutrimouse)

X = as.matrix(nutrimouse$gene)
Y = as.matrix(nutrimouse$lipid)
dim(X) # 40 120
dim(Y) # 40 21

##### start CCA analysis ###################################

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
hist(cca.out$lambda)

# whitened data
CCAX = tcrossprod( scale(X), cca.out$WX )
CCAY = tcrossprod( scale(Y), cca.out$WY )

##### Basic plots ##########################################

# plot correlation matrices
#pdf(file="nutrimouse-matcor.pdf", width=7, height=7)
corplot(cca.out, X,Y)
#dev.off()

# plot canonical correlations
#pdf(file="nutrimouse-barplot.pdf", width=7, height=5)
barplot(cca.out$lambda, xlab = "Dimension", 
  ylab = "Canonical Correlations", names.arg = 1:length(cca.out$lambda))
#dev.off()

##### plot squared correlation loadings ####################

#pdf(file="nutrimouse-loadings.pdf", width=10)
loadplot(cca.out, 5)
#dev.off()

# column sums of squared correlation loadings add to 1
colSums(cca.out$PhiY^2) 

##### scatter plots with labeled data points  ##############

# get labels
g=nutrimouse$genotype
levels(g)
# wt vs ppar
# wild-type and PPARalpha -/- 
# 1 wt
# 2 ppar
table(g)
#wt ppar 
#  20   20

d=nutrimouse$diet
levels(d)
# coc fish lin ref sun

# 1 coconut oil (COC)  black       square
# 2 fish oils (FISH).  red         triangle filled
# 3 linseed oil  (LIN) green       triangle open
# 4 reference diet (REF), blue     circle filled
# 5 sunflower oil (SUN) light blue circle open
table(d)
#coc fish  lin  ref  sun 
#   8    8    8    8    8
bgcol=ifelse(g=="ppar", 0, d) # no fill vs filled

dpch=rep(21, length(d)) # coc = circle
dpch[d=="fish"] = 22 # square
dpch[d=="lin"] = 23 # diamond
dpch[d=="ref"] = 24 # triangle
dpch[d=="sun"] = 25 # reverse triangle

## plot pairs of canonical covariates
#pdf(width=9.1, height=8.3, file="nutrimouse-between.pdf")
par(mfrow=c(2,2))
par(mar=c(4.1, 4.5, 1.9, 2.1))

plot(CCAX[,1], -CCAY[,1], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5), 
   xlab=expression(paste(tilde(X)[1])), ylab=expression(paste(-tilde(Y)[1])),
col=1, pch=21, bg=ifelse(g=="wt", 1, 0) )
legend("bottomright", c("wt", "ppar"), pch=c(21,21), col=c(1,1), pt.bg=c(1,0))
text(-.8, -1.5, "ppar")
text(1.2, 0.5, "wt")

plot(CCAX[,2], -CCAY[,2], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5),
   xlab=expression(paste(tilde(X)[2])), ylab=expression(paste(-tilde(Y)[2])) ,
col=d, pch=dpch)
legend("bottomright", c("COC", "FISH", "LIN", "REF", "SUN"), 
pch=21:25, col=1:5)
text(-1.7, -1.2, "COC")
text(0, -0.6, "SUN / REF")
text(1.3, 0.3, "LIN / FISH /")
text(1.3, 0.6, "SUN /")

plot(CCAX[,3], -CCAY[,3], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5),
   xlab=expression(paste(tilde(X)[3])), ylab=expression(paste(-tilde(Y)[3])) ,
col=d, pch=dpch)
legend("bottomright", c("COC", "FISH", "LIN", "REF", "SUN"), 
pch=21:25, col=1:5)
text(-1.8,-1.1, "SUN")
text(-0.2,-1, "REF")

plot(CCAX[,4], CCAY[,4], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5),
   xlab=expression(paste(tilde(X)[4])), ylab=expression(paste(tilde(Y)[4])) ,
col=d, pch=dpch)
legend("bottomright", c("COC", "FISH", "LIN", "REF", "SUN"), 
pch=21:25, col=1:5)
text(-1.7, -1, "LIN")
text(1.5, 0.7, "FISH")

par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
#dev.off()

## plot first second variates within X and Y

#pdf(width=11, height=5, file="nutrimouse-within.pdf")
par(mfrow=c(1,2))
par(mar=c(4.1, 4.5, 1.9, 2.1))

plot(CCAX[,1], CCAX[,2], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5), 
   xlab=expression(paste(tilde(X)[1])), 
ylab=expression(paste(tilde(X)[2])),
col=d, pch=dpch, bg=bgcol  )
pos = legend("bottomright", c("  COC", "  FISH", "  LIN", "  REF", "  SUN"))
points(x=rep(pos$text$x, times=5) - c(rep(0.25, 5), rep(0.05, 5)), 
    y=rep(pos$text$y, times=5), 
    pch=c(21:25,21:25), col=c(1:5,1:5), bg=c(1:5,0,0,0,0,0) )
legend("bottomleft", c("wt", "ppar"), pch=c(21,21), col=c(1,1), pt.bg=c(1,0))
text(-0.2, -1.5, "COC")
text(-1, 2, "ppar")
text(+1, 2, "wt")

plot(-CCAY[,1], -CCAY[,2], xlim=c(-2.5,2.5), ylim=c(-2.5,2.5), 
   xlab=expression(paste(tilde(-Y)[1])), 
ylab=expression(paste(-tilde(Y)[2])),
col=d, pch=dpch, bg=bgcol  )
pos = legend("bottomright", c("  COC", "  FISH", "  LIN", "  REF", "  SUN"))
points(x=rep(pos$text$x, times=5) - c(rep(0.25, 5), rep(0.05, 5)), 
    y=rep(pos$text$y, times=5), 
    pch=c(21:25,21:25), col=c(1:5,1:5), bg=c(1:5,0,0,0,0,0) )
legend("bottomleft", c("wt", "ppar"), pch=c(21,21), col=c(1,1), pt.bg=c(1,0))
text(-0.2, -1.5, "COC")
text(-1, 2, "ppar")
text(+1, 2, "wt")

par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1))
#dev.off()
