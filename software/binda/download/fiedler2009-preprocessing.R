# 28 February 2015
#
# R script for Gibb and Strimmer- 2015. http://arxiv.org/abs/1502.07959
#
# Preprocess raw mass spectrometry data from Fiedler et al 2009
#


## ----packages------------------------------------------------------------
library("MALDIquant")
library("MALDIquantForeign")
library("downloader")


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
                           halfWindowSize=20)


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


save(avgSpectra, avgSpectra.info, peaks,
     file="discovery.rda")




