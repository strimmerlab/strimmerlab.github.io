# this is a slightly modified version of the file
# https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/inst/doc/nyakas2013.R
# to run the same analysis without requiring prior installation of MALDIquantExamples

## ----setup, echo=TRUE, eval=FALSE----------------------------------------
#  install.packages(c("MALDIquant", "MALDIquantForeign", "downloader"))

## ----packages------------------------------------------------------------
library("MALDIquant")
library("MALDIquantForeign")
library("downloader")

## ----local downloads-----------------------------------------------------
## download the spectra (approx. 48 MB)
download("https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/inst/extdata/nyakas2013/spectra.tar.gz", "nyakas2013spectra.tar.gz")

## ----import--------------------------------------------------------------
## import the spectra
spectra <- import("nyakas2013spectra.tar.gz")

## ----preprocessing-------------------------------------------------------
spectra <- transformIntensity(spectra, method="sqrt")
spectra <- smoothIntensity(spectra, method="SavitzkyGolay",
                           halfWindowSize=10)
spectra <- removeBaseline(spectra, method="SNIP",
                          iterations=10)
spectra <- calibrateIntensity(spectra, method="TIC")

## ----meanspectrum--------------------------------------------------------
meanSpectrum <- averageMassSpectra(spectra)

roi <- detectPeaks(meanSpectrum, SNR=4,
                   halfWindowSize=10)

plot(meanSpectrum, main="Mean Spectrum")
points(roi, col="red")

## ----plotmsihigh---------------------------------------------------------
## find order of peak intensities
o <- order(intensity(roi), decreasing=TRUE)

## plot MSI slice for the highest one
plotMsiSlice(spectra, center=mass(roi)[o[1]], tolerance=0.5)

## ----plotmsimultiple-----------------------------------------------------
plotMsiSlice(spectra, center=mass(roi)[o[2:3]], tolerance=0.5)

## ----plotmsicombine------------------------------------------------------
plotMsiSlice(spectra, center=mass(roi)[o[1:2]], tolerance=0.5,
             combine=TRUE,
             colRamp=list(colorRamp(c("#000000", "#FF00FF")),
                          colorRamp(c("#000000", "#00FF00"))))

## ----msislices-----------------------------------------------------------
slices <- msiSlices(spectra, center=mass(roi), tolerance=0.5)
attributes(slices)

## ----coordinates---------------------------------------------------------
head(coordinates(spectra))
head(coordinates(spectra, adjust=TRUE))

## ----peakim--------------------------------------------------------------
peaks <- detectPeaks(spectra, SNR=3,
                     halfWindowSize=10)
peaks <- binPeaks(peaks)
intMatrix <- intensityMatrix(peaks, spectra)

## ----kmeans--------------------------------------------------------------
km <- kmeans(intMatrix, centers=2)

## ----clustermatrix-------------------------------------------------------
coord <- coordinates(spectra, adjust=TRUE)
maxPixels <- apply(coord, MARGIN=2, FUN=max)
m <- matrix(NA, nrow=maxPixels["x"], ncol=maxPixels["y"])
m[coord] <- km$cluster

## ----plotclusters--------------------------------------------------------
rgbCluster <- function(x) {
  col <- matrix(c(255, 0, 0,
                  0, 255, 0), nrow=2, byrow=TRUE)
  col[x, ]
}
plotMsiSlice(m, colRamp=rgbCluster, scale=FALSE)
