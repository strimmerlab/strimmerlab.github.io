# Reproduces the LUSC TCGA data analysis in
# Jendoubi and Strimmer. 2019. A whitening approach to probabilistic canonical 
# correlation analysis for omics data integration.  BMC Bioinformatics 20:15.
# https://doi.org/10.1186/s12859-018-2572-9


##### load package and data ################################

# load "whitening" library
library("whitening") # contains CCA functions and data

# load data
data(lusc)

X = lusc$rnaseq2
Y = lusc$methyl
dim(X) # 130 206 gene expression
dim(Y) # 130 234 methylation

##### start CCA analysis ###################################

# CCA with shrinkage estimate of joint correlation
cca.out = scca(X, Y, scale=TRUE)
cca.out

# shrinkage intensity
lambda.cor = cca.out$lambda.cor
lambda.cor # 0.1852659

# estimated canonical correlations
cca.out$lambda

sum(cca.out$lambda < 0) # 96

range(cca.out$lambda) # -0.9291533  0.9754134
hist( cca.out$lambda )

# whitened data
CCAX = tcrossprod( scale(X), cca.out$WX )
CCAY = tcrossprod( scale(Y), cca.out$WY )

##### Basic plots ##########################################

# plot correlation matrices
#pdf(file="lusc-matcor.pdf", width=7, height=7)
corplot(cca.out, X, Y)
#dev.off()

# plot canonical correlations
#pdf(file="lusc-barplot.pdf", width=7, height=5)
barplot(cca.out$lambda, xlab = "Dimension", 
  ylab = "Canonical Correlations", names.arg = 1:length(cca.out$lambda))
#dev.off()

##### plot squared correlation loadings ####################

#pdf(file="lusc-loadings.pdf", width=10)
loadplot(cca.out, 10)
#dev.off()

# column sums of squared correlation loadings add to 1
colSums(cca.out$PhiX^2) 

##### scatter plots with labeled data points  ##############

# get labels
colpacks = rep(1, 130)       # black 90 or less
colpacks[ lusc$packs > 90] = 2    # red > 90
colpacks[ is.na(lusc$packs) ] = 3 # unknown
table(colpacks)

ppch = rep(21, length(lusc$sex)) # circle 90 or less
ppch[is.na(lusc$packs)] = 22 # reverse triangle   20
ppch[lusc$packs > 90] = 25 # square

#pdf(width=11, height=5,file="lusc-scatter.pdf")
par(mfrow=c(1,2))
par(mar=c(4.1, 4.5, 1.9, 2.1))

plot(CCAX[,1], CCAY[,1], xlim=c(-2.75,2.75), ylim=c(-2.75,2.75), 
   xlab=expression(paste(tilde(X)[1])), 
   ylab=expression(paste(tilde(Y)[1])),
   col=1, pch=21, bg=ifelse(lusc$sex=="male", 1, 0) )
legend("bottomright", c("male", "female"), pch=c(21,21), 
   col=c(1,1), pt.bg=c(1,0))
text(1, 0, "male")
text(-2, -1, "female")

plot(CCAX[,1], CCAX[,2], xlim=c(-2.75,2.75), ylim=c(-2.75,2.75), 
xlab=expression(paste(tilde(X)[1])), 
ylab=expression(paste(tilde(X)[2])),
col=colpacks, pch=ppch, bg=ifelse(lusc$sex=="male", colpacks, 0)  )
text(1.8, -0.5, "male")
text(-2, 1.8, "female")
pos = legend("bottomleft", c(expression(paste("  ", "">90, " packs")), 
     expression(paste("  ", ""<=90, " packs")), "  unknown packs"))
points(x=rep(pos$text$x, times=2) - c(0.25, 0.25, 0.25, 0.05, 0.05, 0.05), 
    y=rep(pos$text$y, times=2), pch=c(25,21,22,25,21,22), 
    col=c(2,1,3,2,1,3), bg=c(2,1,3,0,0,0) )

par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mfrow=c(1,1))
#dev.off()

##### survival plots #######################################

require("survival")
s = Surv(lusc$survivalTime, lusc$censoringStatus)
cox = coxph(s ~ lusc$sex)
n.class <- 2
pv <- 1 - pchisq(2 * (cox$loglik[2] - cox$loglik[1]), df = n.class - 1)

#pdf(file="lusc-surv.pdf")
plot(survfit(s ~ lusc$sex), xlab = "Years", 
  ylab = "Probability of survival", col = c(1,1), lty=c(2,1), lwd=c(2,2))
legend("topright", legend = c("male", "female"), col = c(1,1), lty =c(1,2), 
  lwd=c(2,2), box.lwd=1)
text(10.3, 0.7, paste("p =", as.character(round(pv, 4))))
#dev.off()
