# 28 February 2015
#
# R script for Gibb and Strimmer- 2015. http://arxiv.org/abs/1502.07959
#
# Analyze calibrated spectrometry data from Fiedler et al 2009
# 
# Note the script below reproduces all figures from the paper.


## ----packages------------------------------------------------------------

library("MALDIquant")
library("binda")
library("crossval")


## ------------------------------------------------------------------------
## load preprocessed data
load("discovery.rda")


Xtrain <- intensityMatrix(peaks, avgSpectra)
rownames(Xtrain) <- avgSpectra.info$patientID
dim(Xtrain) #  40 166

Ytrain <- avgSpectra.info$health
table(Ytrain)
#Ytrain
# cancer control 
#     20      20


# absence/presence matrix
sum(is.na(intensityMatrix(peaks))) # 1741
mean( apply(is.na(intensityMatrix(peaks)), 1, sum) ) 
# 43.525  average number of peaks missing per sample (of 166) = 26 percent

Xtrain.b.naive = ifelse(is.na(intensityMatrix(peaks)), 0, 1)
rownames(Xtrain.b.naive) <- avgSpectra.info$patientID

# m/z values
mz.full = as.double(colnames(Xtrain))
mz = round( mz.full )
any(duplicated(mz)) # FALSE
mz
# [1] 1012 1021 1029 1041 1058 1078 1084 1102 1114 1132 1158 1176 1182 1187 1200
# [16] 1207 1225 1245 1255 1264 1299 1329 1345 1352 1372 1390 1420 1450 1466 1494
# [31] 1520 1530 1538 1546 1567 1586 1617 1628 1640 1659 1741 1780 1855 1866 1886
# [46] 1897 1928 1945 2023 2082 2093 2106 2154 2210 2281 2295 2348 2381 2424 2468
# [61] 2496 2546 2555 2604 2644 2661 2673 2685 2732 2740 2756 2770 2791 2846 2863
# [76] 2903 2917 2933 2953 2979 2991 3097 3143 3159 3193 3209 3218 3225 3242 3264
# [91] 3280 3317 3367 3378 3449 3525 3541 3586 3769 3884 3920 3935 4055 4072 4092
#[106] 4111 4124 4153 4170 4195 4211 4236 4251 4267 4303 4468 4495 4531 4615 4646
#[121] 4672 4757 4788 4965 5005 5047 5133 5250 5267 5295 5338 5377 5392 5807 5864
#[136] 5906 5946 5960 6051 6091 6307 6335 6381 6435 6633 6670 7012 7568 7768 7809
#[151] 7821 7926 8131 8144 8184 8606 8868 8937 8989 9066 9137 9181 9293 9336 9384
#[166] 9428

## -----------------------------------------------------------------------
## dichotomize data

thr = optimizeThreshold(Xtrain, Ytrain)

Xtrain.b = dichotomize(Xtrain, thr)
colnames(Xtrain.b) = mz
is.binaryMatrix(Xtrain.b) # TRUE


## -----------------------------------------------------------------------
# visualize effect of dichotomization by clustering

#' Modify the leafs nodes according to group label:
symbolCodes=c(P=19, C=1)

labelCol = function(x) {
  if (is.leaf(x)) {
    ## fetch label
    s = attr(x, "label")
    label = substr(s, 2, 2) # second letter (P/C) indicates Cancer or Control

    ## assign symbol
    attr(x, "nodePar") = list(pch=symbolCodes[label] ) 

  }
  return(x)
}


#' Compute distance matrix:
dmat = dist(Xtrain.b, method="binary")
dmat.naive = dist(Xtrain.b.naive, method="binary")

#' Hierarchical clustering:
hc = dendrapply(as.dendrogram(hclust( dmat , method="ward.D2")), labelCol)
hc.naive = dendrapply(as.dendrogram(hclust( dmat.naive , method="ward.D2")), labelCol)


#+ fig.width=12, fig.height=7
pdf(file="fig1-clustering.pdf")
par(mfrow=c(2,1))
par(mar=c(2.5, 2, 2, 0) )
plot(hc.naive, main="A) Absence/Presence Matrix")
plot(hc, main="B) Dichotomized Data")
par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1)) # back to default
dev.off()



###----------------------------------------------------------------------
## rank features

br = binda.ranking(Xtrain.b, Ytrain)

br[1:10,]
#    idx    score  t.cancer t.control
#4495 117 36.19048  6.015852 -6.015852
#8868 157 36.19048  6.015852 -6.015852
#8989 159 36.19048  6.015852 -6.015852
#1855  43 32.72727  5.720776 -5.720776
#4468 116 32.72727  5.720776 -5.720776
#8937 158 32.72727  5.720776 -5.720776
#2023  49 32.40000  5.692100 -5.692100
#1866  44 29.56522  5.437391 -5.437391
#5864 135 26.66667 -5.163978  5.163978
#5946 137 26.66667 -5.163978  5.163978

# peaks #1-3 have same rank
# peaks #4-6 have same rank

pdf(file="fig2-ranking.pdf")
plot(br, top=30, arrow.col="black", zeroaxis.col="black", ylab="Peaks (m/z)",
      main="30 Most Differentially Expressed Peaks")
dev.off()


# our 5 peaks (pancreas growth factor)

# pairs: 4468, 8937  central peak  (#4-6)
#        4495, 8989  right shoulder (#1-3)
#              8868 (#1-#3) left shoulder (#1-3)   (4434 lost in 50% filtering)


ourPeaks = c(116, 117, 157,158,159)  
mz[ourPeaks]


# ranking of platelet peaks
which ( grepl("3884|7768", rownames(br)) )  # 149 (148-151)  161 (group 157-163) (of 166)


## --------------------------------------------------------------------------
## plot of the 5 five peaks

mz.full[ourPeaks]
# 4468.066 4494.803 8868.268 8936.972 8989.204

left=4420
right=4510
s=35


pdf(file="fig3-peaks.pdf", width=7, height=5)
par(mar=c(2, 3.9, 2, 0.2) )

par(mfrow=c(1,2))
plot(avgSpectra[[s]], xlim=c(left, right), ylim=c(0, 0.0009), main="A) Double Charge", sub="", xlab="m/z")
points(peaks[[s]], col="black", pch=4)
labelPeaks(peaks[[s]], mass=mz[ourPeaks] , col="black", cex=1)


plot(avgSpectra[[s]], xlim=c(2*left, 2*right), ylim=c(0, 0.0009), main="B) Single Charge", sub="", xlab="m/z", ylab="")
points(peaks[[s]], col="black", pch=4)
labelPeaks(peaks[[s]], mass=mz.full[ourPeaks] , col="black", cex=1)

par(mfrow=c(1,1))
par(mar=c(5.1, 4.1, 4.1, 2.1)) # back to default

dev.off()



## --------------------------------------------------------------------------
## crossvalidation analysis

# predict using specified peaks
predfun1 = function(Xtrain, Ytrain, Xtest, Ytest, selPeaks)
{
  binda.out = binda(Xtrain[, selPeaks, drop=FALSE], Ytrain, verbose=FALSE)
  ynew = predict.binda(binda.out, Xtest[, selPeaks, drop=FALSE], verbose=FALSE)$class 

  cm = confusionMatrix(Ytest, ynew, negative="control") 
 
  return(cm)  
}


# predict using top ranked peaks
predfun2 = function(Xtrain, Ytrain, Xtest, Ytest, numPeaks)
{
  bir = binda.ranking(Xtrain, Ytrain, verbose=FALSE)
  selPeaks = bir[,"idx"][1:numPeaks]

  cm = predfun1(Xtrain, Ytrain, Xtest, Ytest, selPeaks)

  return(cm)  
}



#' We use 5-fold cross-validation (i.e. divide into 5 folds of 4 samples each )
#' and repeat 20 times
 
K=5
B=20


#' Find optimal number of peaks (we compute errors for various numbers
#' of top ranked genes)

set.seed(12345)
numPeaks = c(1:10, 15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 166)
simFun = function(i)
  {
    cat("Number of Peaks:", i, "\n")
    cvp = crossval(predfun2, Xtrain.b, Ytrain, K=K, B=B, numPeaks = i, verbose=FALSE)
    return( diagnosticErrors(cvp$stat) )
  }

#' this may take some time:
cvsim = lapply(numPeaks, simFun)
cvsim = do.call(rbind, cvsim)
binda.sim = cbind(numPeaks,cvsim)

#' This shows accuracy, sensitivity, specificity, positive predictive value,
#' negative predictive value, and log-odds ratio 
#' (for definitions see help page ?diagnosticErrors)
binda.sim
#      numPeaks     acc   sens   spec       ppv       npv      lor
# [1,]        1 0.89125 0.8725 0.9100 0.9064935 0.8771084 4.236881
# [2,]        2 0.92125 0.9425 0.9000 0.9040767 0.9399478 4.993976
# [3,]        3 0.93000 0.9425 0.9175 0.9195122 0.9410256 5.205605
# [4,]        4 0.95000 0.9425 0.9575 0.9568528 0.9433498 5.911573
# [5,]        5 0.96000 0.9500 0.9700 0.9693878 0.9509804 6.420538
# [6,]        6 0.96125 0.9500 0.9725 0.9718670 0.9511002 6.510123
# [7,]        7 0.96375 0.9500 0.9775 0.9768638 0.9513382 6.715922
# [8,]        8 0.96500 0.9500 0.9800 0.9793814 0.9514563 6.836259
# [9,]        9 0.95625 0.9550 0.9575 0.9573935 0.9551122 6.169870
#[10,]       10 0.95750 0.9525 0.9625 0.9621212 0.9529703 6.243554
#[11,]       15 0.95250 0.9550 0.9500 0.9502488 0.9547739 5.999488
#[12,]       20 0.95875 0.9600 0.9575 0.9576060 0.9598997 6.292875
#[13,]       25 0.95625 0.9525 0.9600 0.9596977 0.9528536 6.176414
#[14,]       30 0.95125 0.9500 0.9525 0.9523810 0.9501247 5.942799
#[15,]       40 0.95750 0.9625 0.9525 0.9529703 0.9621212 6.243554
#[16,]       50 0.95625 0.9625 0.9500 0.9506173 0.9620253 6.189632
#[17,]       75 0.95875 0.9675 0.9500 0.9508600 0.9669211 6.337914
#[18,]      100 0.97125 0.9925 0.9500 0.9520384 0.9921671 7.829763
#[19,]      125 0.97125 0.9925 0.9500 0.9520384 0.9921671 7.829763
#[20,]      150 0.97375 1.0000 0.9475 0.9501188 1.0000000      Inf
#[21,]      166 0.97625 1.0000 0.9525 0.9546539 1.0000000      Inf

save(binda.sim, file="binda.sim.rda")

#' Estimation of various errors using the our 5 peaks 
set.seed(12345)
cvp = crossval(predfun1, Xtrain.b, Ytrain, K=K, B=B, selPeaks=ourPeaks, verbose=FALSE)
cvp$stat
# FP   TP   TN   FN 
#0.14 3.80 3.86 0.20 
diagnosticErrors(cvp$stat)
#      acc      sens      spec       ppv       npv       lor
#0.9575000 0.9500000 0.9650000 0.9644670 0.9507389 6.2612190


##----------------------------------------------------------------------
# show densities with estimated thresholds


plotPeakHist = function(idx)
{
  isControl = (Ytrain == "control")
  isCancer = !isControl
  alpha = sum(isControl)/length(isControl)
  beta=1-alpha


  xlim = c(0, max(Xtrain[,idx]))
  
  d0 = density(Xtrain[,idx]) 
  keep = ( d0$x >= xlim[1] & d0$x <= xlim[2] )
  plot(d0$x[keep], d0$y[keep], xlab = "Protein Expression", main = mz[idx], xlim=xlim, type="l")

  #plot( density(Xtrain[,idx])  , xlab = "Protein Expression", main = mz[idx], xlim=xlim, zero.line=FALSE )


  #h2 = hist(Xtrain[ isCancer, idx], plot=FALSE)
  #h2$density = h2$density*beta
  #plot(h2, freq=FALSE, col=1, add=TRUE, xlim=xlim, density=15, angle=135)
  d2 = density(Xtrain[ isCancer,idx] )
  keep = ( d2$x >= xlim[1] & d2$x <= xlim[2] )
  lines( d2$x[keep], d2$y[keep]*beta , col=1, lty=2, lwd=2)


  #h1 = hist(Xtrain[ isControl,idx], plot=FALSE)
  #h1$density = h1$density*alpha
  #plot(h1, freq=FALSE, col=1, add=TRUE, xlim=xlim, density=15, angle=45)
  #d1 = density(Xtrain[ isControl,idx] )
  #keep = ( d1$x >= xlim[1] & d1$x <= xlim[2] )
  #lines( d1$x[keep], d1$y[keep]*alpha , col=1, lty=3, lwd=1)

  abline(v=thr[idx], col=1, lwd=2)

}



#plotPeakHist(116) # peak 4468.066


pdf(file="fig4-thresholds.pdf", width=9, height=6)

par(mfrow=c(2,3))

plot(NA) # 4434  
legend(x="center", c("Density of all samples", "Density of cancer samples", "Estimated threshold"), 
        lwd=c(1,2, 2),
        lty = c(1, 2, 1),
        cex=1.3)


plotPeakHist(116) # peak 4468
plotPeakHist(117) # peak 4495

plotPeakHist(157) # peak 8868
plotPeakHist(158) # peak 8937
plotPeakHist(159) # peak 8989 

par(mfrow=c(1,1))

dev.off()



