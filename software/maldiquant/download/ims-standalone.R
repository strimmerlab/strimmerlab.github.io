# this is a slightly modified version of the file
# https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/inst/doc/ims.R
# to run the same analysis without requiring prior installation of MALDIquantExamples


## ----setup, echo=TRUE, eval=FALSE----------------------------------------
## install.packages(c("MALDIquant", "MALDIquantForeign", "downloader"))


## ----packages------------------------------------------------------------
library("MALDIquant")
library("MALDIquantForeign")
library("downloader")


## -- local downloads -----------------------------------------------------
## download the spectra (approx. 3 MB)
download("http://www.maldi-msi.org/download/imzml/s043_processed.zip", "s043_processed.zip")

## download plot function
download("https://raw.githubusercontent.com/sgibb/MALDIquantExamples/master/R/plotImsSlice.R", "plotImsSlice.R")


## ----import--------------------------------------------------------------
spectra <- import("s043_processed.zip", verbose=FALSE)


## ----plotimage-----------------------------------------------------------
source("plotImsSlice.R")
plotImsSlice(spectra, range=c(156.95, 157.45), main="urinary bladder")



