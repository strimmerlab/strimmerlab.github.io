# /*
# This is an R script containing R markdown comments.  It can be run as is in R.
# To generate a document containing the formatted R code, R output and markdown 
# click the "Compile Notebook" button in R Studio, or run the command
# rmarkdown::render() - see http://rmarkdown.rstudio.com/r_notebook_format.html
# */


#' ---
#' title: "Analysis of Dorothea Data Set"
#' output: pdf_document
#' author: "Sebastian Gibb and Korbinian Strimmer"
#' date:   30 April 2015
#' ---


#' 
#' #  Dorothea data set
#'
#' The Dorothea data set in R data format can be downloaded from
#' https://strimmerlab.github.io/data/dorothea.rda

#' 
#' Load data set
load("dorothea.rda")

#' Training data:
dim(x.train) # 800 samples, 100000 features
table(y.train)

#' Validation data:
dim(x.valid) # 350 samples, 100000 features
table(y.valid)

#' 
#' #  Ranking of features according to binda

#' Load binda R package
library("binda")

#' Compute ranking:
br = binda.ranking(x.train, y.train, verbose=TRUE)

#' List the 20 top ranking predictors
br[1:20,]
plot(br, top=20)

#' Plot ranks:
par(mfrow=c(1,2))
plot(br[,"score"], type="l", ylab="Ranking Score", xlab = "Rank (1 to 100000)")
plot(br[,"score"], type="l", xlim=c(1, 1000), ylab="Ranking Score", xlab = "Rank (1 to 1000)")
par(mfrow=c(1,1))


#' 
#' #  Cross-validation analysis


#' Function zo compute test accuracy:
predfun = function(Xtrain, Ytrain, Xtest, Ytest)
{
   # learn predictor
   binda.fit = binda(Xtrain, Ytrain, lambda.freq=0, verbose=FALSE)

   # predict classes using new test data
   yhat = predict(binda.fit, Xtest, verbose=FALSE)$class

   acc = ( sum( yhat == Ytest )/length(yhat) )

   return(acc)
}


#' Use cross-validation to find optimal number of predictors:

library("crossval")

set.seed(12345)
numPreds = c(1:10,20, 50, 100 )
simFun = function(numpred)
  {
    cat("Number of Predictors:", numpred, "\n")

    predlist = br[1:numpred, "idx"]
    x.train2 = x.train[,predlist, drop=FALSE ]
    x.valid2 = x.valid[,predlist, drop=FALSE ]
    valError = predfun(x.train2, y.train, x.valid2, y.valid)
    cv.out = crossval(predfun, x.train2, y.train, K=5, B=20, verbose=FALSE)

    return( c(valError=valError, ACC=cv.out$stat, ACC.se = cv.out$stat.se) )

  }

#' This may take some time:
cvsim = lapply(numPreds, simFun)
cvsim = do.call(rbind, cvsim)
binda.sim = cbind(numPreds,cvsim)
binda.sim

#' Validation error if all 100000 predictors are included:
valErrorAll = predfun(x.train, y.train, x.valid, y.valid)
valErrorAll


#' 
#' #  Comparison with Random Forest

library("randomForest")

if( file.exists("rf.rda") )
  {
  load("rf.rda") # load precomputed random forest
}
if( !file.exists("rf.rda") )
{
  set.seed(12345)
  rf.fit = randomForest(x.train, y=as.factor(y.train), ntree=100, importance=TRUE)
  save(rf.fit, file="rf.rda")
}

#' Ranking of predictors using training data
varimp = rf.fit$importance[,4]    # MeanDecreaseGini
names(varimp) = NULL
ovi = order(varimp, decreasing=TRUE)
idx = ovi[1:20]
cbind(idx, MeanDecreaseGini=varimp[idx])


#' Test error
yhat = predict(rf.fit, x.valid)
acc = ( sum( yhat == y.valid )/length(yhat) )
acc
