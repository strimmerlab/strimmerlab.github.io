# this is a slightly modified version of the file
# https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/inst/doc/fiedler2009.R
# to run the same analysis without requiring prior installation of MALDIquantExamples


## ----setup, echo=TRUE, eval=FALSE----------------------------------------
## install.packages(c("MALDIquant", "MALDIquantForeign", "downloader", 
##                    "sda", "crossval")


## ----packages------------------------------------------------------------
library("MALDIquant")
library("MALDIquantForeign")
library("downloader")
library("sda")
library("crossval")


## -- local download of Fiedler et al. 2009 data --------------------------
## download the spectra (approx. 90 MB)
download("https://github.com/sgibb/MALDIquantExamples/blob/master/inst/extdata/fiedler2009/spectra.tar.gz?raw=true", "fiedler2009spectra.tar.gz")

## download metadata
download("https://github.com/sgibb/MALDIquantExamples/blob/master/inst/extdata/fiedler2009/spectra_info.csv?raw=true", "fiedler2009info.csv")


## ----import--------------------------------------------------------------
## import the spectra
spectra <- import("fiedler2009spectra.tar.gz", verbose=FALSE)

## import metadata
spectra.info <- read.table("fiedler2009info.csv", sep=",", header=TRUE)


## ----reduce--------------------------------------------------------------
isHeidelberg <- spectra.info$location == "heidelberg"

spectra <- spectra[isHeidelberg]
spectra.info <- spectra.info[isHeidelberg,]


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


## ----sm------------------------------------------------------------------
spectra <- smoothIntensity(spectra, method="SavitzkyGolay",
                           halfWindowSize=10)


## ----be------------------------------------------------------------------
baseline <- estimateBaseline(spectra[[1]], method="SNIP",
                             iterations=150)
plot(spectra[[1]])
lines(baseline, col="red", lwd=2)


## ----bc------------------------------------------------------------------
spectra <- removeBaseline(spectra, method="SNIP",
                          iterations=150)
plot(spectra[[1]])


## ----cb------------------------------------------------------------------
spectra <- calibrateIntensity(spectra, method="TIC")


## ----pa------------------------------------------------------------------
spectra <- alignSpectra(spectra)


## ----avg-----------------------------------------------------------------
avgSpectra <-
  averageMassSpectra(spectra, labels=spectra.info$patientID)
avgSpectra.info <-
  spectra.info[!duplicated(spectra.info$patientID), ]


## ----noise---------------------------------------------------------------
noise <- estimateNoise(avgSpectra[[1]])
plot(avgSpectra[[1]], xlim=c(4000, 5000), ylim=c(0, 0.002))
lines(noise, col="red")                     # SNR == 1
lines(noise[, 1], 2*noise[, 2], col="blue") # SNR == 2


## ----pd------------------------------------------------------------------
peaks <- detectPeaks(avgSpectra, SNR=2, halfWindowSize=20)


## ----pdp-----------------------------------------------------------------
plot(avgSpectra[[1]], xlim=c(4000, 5000), ylim=c(0, 0.002))
points(peaks[[1]], col="red", pch=4)


## ----pb------------------------------------------------------------------
peaks <- binPeaks(peaks)


## ----pf------------------------------------------------------------------
peaks <- filterPeaks(peaks, minFrequency=c(0.5, 0.5),
                     labels=avgSpectra.info$health,
                     mergeWhitelists=TRUE)


## ----fm------------------------------------------------------------------
featureMatrix <- intensityMatrix(peaks, avgSpectra)
rownames(featureMatrix) <- avgSpectra.info$patientID


## ----dda-----------------------------------------------------------------
Xtrain <- featureMatrix
Ytrain <- avgSpectra.info$health
ddar <- sda.ranking(Xtrain=featureMatrix, L=Ytrain, fdr=FALSE,
                    diagonal=TRUE)


## ----ddaresults, echo=FALSE, results="asis"------------------------------
ddar[1:10, ]


## ----hclust--------------------------------------------------------------
distanceMatrix <- dist(featureMatrix, method="euclidean")

hClust <- hclust(distanceMatrix, method="complete")

plot(hClust, hang=-1)


## ----hclustfs------------------------------------------------------------
top <- ddar[1:2, "idx"]

distanceMatrixTop <- dist(featureMatrix[, top],
                          method="euclidean")

hClustTop <- hclust(distanceMatrixTop, method="complete")

plot(hClustTop, hang=-1)


## ----cv------------------------------------------------------------------
# create a prediction function for the cross validation
predfun.dda <- function(Xtrain, Ytrain, Xtest, Ytest,
                        negative) {
  dda.fit <- sda(Xtrain, Ytrain, diagonal=TRUE, verbose=FALSE)
  ynew <- predict(dda.fit, Xtest, verbose=FALSE)$class
  return(confusionMatrix(Ytest, ynew, negative=negative))
}

# set seed to get reproducible results
set.seed(1234)

cv.out <- crossval(predfun.dda,
                   X=featureMatrix[, top],
                   Y=avgSpectra.info$health,
                   K=10, B=20,
                   negative="control",
                   verbose=FALSE)
diagnosticErrors(cv.out$stat)



