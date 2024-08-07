# this is a slightly modified version of the file
# https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/inst/doc/species.R
# to run the same analysis without requiring prior installation of MALDIquantExamples


## ----setup, echo=TRUE, eval=FALSE----------------------------------------
#  install.packages(c("MALDIquant", "MALDIquantForeign", "downloader", 
#                   "pvclust", "sda", "crossval"))


## ----packages------------------------------------------------------------
library("MALDIquant")
library("MALDIquantForeign")
library("downloader")
library("pvclust")
library("sda")
library("crossval")


## -- local download of species data --------------------------
## download the spectra (approx. 4 MB)
download("https://github.com/sgibb/MALDIquantExamples/blob/master/inst/extdata/species/spectra.tar.gz?raw=true", "speciesSpectra.tar.gz")

## ----import--------------------------------------------------------------
spectra <- import("speciesSpectra.tar.gz", verbose=FALSE)

## ----qc------------------------------------------------------------------
table(sapply(spectra, length))
any(sapply(spectra, isEmpty))
all(sapply(spectra, isRegular))

## ----trim----------------------------------------------------------------
spectra <- trim(spectra)

## ----plotseed, echo=FALSE------------------------------------------------
set.seed(123)

## ----plot----------------------------------------------------------------
idx <- sample(length(spectra), size=2)
plot(spectra[[idx[1]]])
plot(spectra[[idx[2]]])

## ----vs------------------------------------------------------------------
spectra <- transformIntensity(spectra, method="sqrt")

## ----fwhm----------------------------------------------------------------
plot(spectra[[1]], type="b",
     xlim=c(2235.3, 2252.0), ylim=c(45, 100))
abline(h=72, col=4, lty=2)
plot(spectra[[1]], type="b",
     xlim=c(11220, 11250), ylim=c(24, 40))
abline(h=32, col=4, lty=2)

## ----sm------------------------------------------------------------------
spectra <- smoothIntensity(spectra, method="SavitzkyGolay",
                           halfWindowSize=10)

## ----be------------------------------------------------------------------
## define iteration steps: 25, 50, ..., 100
iterations <- seq(from=25, to=100, by=25)
## define different colors for each step
col <- rainbow(length(iterations))

plot(spectra[[1]], xlim=c(2000, 12000))

## draw different baseline estimates
for (i in seq(along=iterations)) {
  baseline <- estimateBaseline(spectra[[1]], method="SNIP",
                               iterations=iterations[i])
  lines(baseline, col=col[i], lwd=2)
}

legend("topright", legend=iterations, col=col, lwd=1)

## ----bc------------------------------------------------------------------
spectra <- removeBaseline(spectra, method="SNIP",
                          iterations=25)
plot(spectra[[1]])

## ----cb------------------------------------------------------------------
spectra <- calibrateIntensity(spectra, method="TIC")

## ----pa------------------------------------------------------------------
spectra <- alignSpectra(spectra)

## ----metadata------------------------------------------------------------
metaData(spectra[[1]])$spot

## ----spots---------------------------------------------------------------
spots <- sapply(spectra, function(x)metaData(x)$spot)
species <- sapply(spectra, function(x)metaData(x)$sampleName)
head(spots)
head(species)

## ----average-------------------------------------------------------------
avgSpectra <-
  averageMassSpectra(spectra, labels=paste0(species, spots))

## ----noise---------------------------------------------------------------
## define snrs steps: 1, 1.5, ... 2.5
snrs <- seq(from=1, to=2.5, by=0.5)
## define different colors for each step
col <- rainbow(length(snrs))

## estimate noise
noise <- estimateNoise(avgSpectra[[1]],
                       method="SuperSmoother")

plot(avgSpectra[[1]],
     xlim=c(6000, 16000), ylim=c(0, 0.0016))

for (i in seq(along=snrs)) {
  lines(noise[, "mass"],
        noise[, "intensity"]*snrs[i],
        col=col[i], lwd=2)
}
legend("topright", legend=snrs, col=col, lwd=1)

## ----pd------------------------------------------------------------------
peaks <- detectPeaks(avgSpectra, SNR=2, halfWindowSize=10)

## ----pdp-----------------------------------------------------------------
plot(avgSpectra[[1]], xlim=c(6000, 16000), ylim=c(0, 0.0016))
points(peaks[[1]], col="red", pch=4)

## ----pb------------------------------------------------------------------
peaks <- binPeaks(peaks)

## ----pf------------------------------------------------------------------
peaks <- filterPeaks(peaks, minFrequency=0.25)

## ----spots2--------------------------------------------------------------
spots <- sapply(avgSpectra, function(x)metaData(x)$spot)
species <- sapply(avgSpectra, function(x)metaData(x)$sampleName)
species <- factor(species) # convert to factor
                           # (needed later in crossval)

## ----fm------------------------------------------------------------------
featureMatrix <- intensityMatrix(peaks, avgSpectra)
rownames(featureMatrix) <- paste(species, spots, sep=".")

## ----clust, eval=FALSE, fig.height=5-------------------------------------
#  pv <- pvclust(t(featureMatrix),
#                method.hclust="ward",
#                method.dist="euclidean")
#  plot(pv, print.num=FALSE)

## ----clusthidden, echo=FALSE, fig.height=5-------------------------------
## suppress messages "ward is renamed into ward.D"; BTW we want to use ward.D2.
## I asked the authors of pvclust to allow ward.D2.
pv <- suppressMessages(pvclust(t(featureMatrix),
                       method.hclust="ward",
                       method.dist="euclidean"))
plot(pv, print.num=FALSE)

## ----dda, fig.height=7.5-------------------------------------------------
ddar <- sda.ranking(Xtrain=featureMatrix, L=species,
                    fdr=FALSE, diagonal=TRUE)
plot(ddar)

## ----lda, fig.height=7.5-------------------------------------------------
ldar <- sda.ranking(Xtrain=featureMatrix, L=species,
                    fdr=FALSE, diagonal=FALSE)
plot(ldar)

## ----predictfuncv--------------------------------------------------------
predfun <- function(Xtrain, Ytrain, Xtest, Ytest,
                    numVars, diagonal=FALSE) {
  # estimate ranking and determine the best numVars variables
  ra <- sda.ranking(Xtrain, Ytrain,
                    verbose=FALSE, diagonal=diagonal, fdr=FALSE)
  selVars <- ra[,"idx"][1:numVars]

  # fit and predict
  sda.out <- sda(Xtrain[, selVars, drop=FALSE], Ytrain,
                 diagonal=diagonal, verbose=FALSE)
  ynew <- predict(sda.out, Xtest[, selVars, drop=FALSE],
                  verbose=FALSE)$class

  # compute accuracy
  acc <- mean(Ytest == ynew)

  return(acc)
}

## ----cvsetup-------------------------------------------------------------
K <- 5  # number of folds
B <- 20 # number of repetitions

## ----cvtop10-------------------------------------------------------------
set.seed(12345)
cv.dda10 <- crossval(predfun,
                     X=featureMatrix, Y=species,
                     K=K, B=B,
                     numVars=10, diagonal=FALSE,
                     verbose=FALSE)
cv.dda10$stat

## ----cvoptimaldd---------------------------------------------------------
npeaks <- c(1:15, ncol(featureMatrix))  # number of peaks

## ----cvoptimaldda--------------------------------------------------------
# estimate accuracy for DDA
set.seed(12345)
cvsim.dda <- sapply(npeaks, function(i) {
  cv <- crossval(predfun,
                 X=featureMatrix, Y=species,
                 K=K, B=B, numVars=i, diagonal=TRUE,
                 verbose=FALSE)
  return(cv$stat)
})

## ----cvoptimallda--------------------------------------------------------
# estimate accuracy for LDA
set.seed(12345)
cvsim.lda <- sapply(npeaks, function(i) {
  cv <- crossval(predfun,
                 X=featureMatrix, Y=species,
                 K=K, B=B, numVars=i, diagonal=FALSE,
                 verbose=FALSE)
  return(cv$stat)
})

## ----cvoptimaltable------------------------------------------------------
result.sim <- cbind(nPeaks=npeaks,
                    "DDA-ACC"=cvsim.dda,
                    "LDA-ACC"=cvsim.lda)
result.sim


