### last update: 15 April 2014 

###
### this example requires "sda" in version 1.3.2 (or later)
### and "crossval" in version 1.0.0 (or later)
###


library("sda")

#############################################
data(khan2001)

# create data set containing only the SRBCT samples
del.idx = which( khan2001$y == "non-SRBCT" )
srbct.x = khan2001$x[-del.idx,]
srbct.y = factor(khan2001$y[-del.idx])
dim(srbct.x)
levels(srbct.y)

# divide into training and test data
Xtrain = srbct.x[1:63,]
Ytrain = srbct.y[1:63]
Xtest = srbct.x[64:83,]
Ytest = srbct.y[64:83]

##############################################
## DDA analysis ##############################
##############################################

### step 1 - feature ranking

# ranking by averaged squared t-scores across the four groups
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, diagonal=TRUE, ranking.score="avg")
sum( ra[, "lfdr"]< 0.80)  # 97 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  145 genes according to HC criterion

# ranking by maximum of squared t-scores across the four groups
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, diagonal=TRUE, ranking.score="max")
sum( ra[, "lfdr"]< 0.80)  # 78 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  121 genes according to HC criterion

# ranking by mutual information (weighted sum of squared t-scores)
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, diagonal=TRUE, ranking.score="entropy")
sum( ra[, "lfdr"]< 0.80)  # 99 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  158 genes according to HC criterion

##

# pick the top 99 genes
plot(ra, top=99, main="The 99 Top Ranking Genes", ylab="Gene ID")
idx = ra[1:99,"idx"]
Xtrain2 = Xtrain[,idx]
Xtest2 = Xtest[,idx]

### step 2 - training the classifier

sda.fit = sda(Xtrain2, Ytrain, diagonal=TRUE)

### step 3 - prediction
predict(sda.fit, Xtest2)
ynew = predict(sda.fit, Xtest2)$class
sum(ynew != Ytest) # 1


##############################################
## LDA analysis ##############################
##############################################

### step 1 - feature ranking

# ranking by averaged squared cat-scores across the four groups
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, ranking.score="avg")
sum( ra[, "lfdr"]< 0.80)  # 93 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  143 genes according to HC criterion

# ranking by maximum of squared cat-scores across the four groups
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, ranking.score="max")
sum( ra[, "lfdr"]< 0.80)  # 156 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  194 genes according to HC criterion

# ranking by mutual information (weighted sum of squared cat-scores)
ra = sda.ranking(Xtrain, Ytrain, fdr=TRUE, plot.fdr=TRUE, ranking.score="entropy")
sum( ra[, "lfdr"]< 0.80)  # 97 genes included in classifier (by FNDR control)
which.max( ra[, "HC"] )  #  140 genes according to HC criterion

##

# pick the top 97 genes
plot(ra, top=97, main="The 97 Top Ranking Genes", ylab="Gene ID")
idx = ra[1:97,"idx"]
Xtrain2 = Xtrain[,idx]
Xtest2 = Xtest[,idx]

### step 2 - training the classifier

sda.fit = sda(Xtrain2, Ytrain)

### step 3 - prediction
predict(sda.fit, Xtest2)
ynew = predict(sda.fit, Xtest2)$class
sum(ynew != Ytest) # 0



############################################
# estimate accuracy using cross-validation #
############################################

library("crossval")

# predict using the numVars top ranked variables specified
predfun = function(Xtrain, Ytrain, Xtest, Ytest, numVars, diagonal=FALSE,
    ranking.score="entropy")
{ 
  # estimate ranking and determine the best numVars variables
  ra = sda.ranking(Xtrain, Ytrain, verbose=FALSE, diagonal=diagonal, 
        fdr=FALSE, ranking.score=ranking.score)
  selVars = ra[,"idx"][1:numVars]

  # fit and predict
  sda.out = sda(Xtrain[, selVars, drop=FALSE], Ytrain, diagonal=diagonal,
               verbose=FALSE)
  ynew = predict(sda.out, Xtest[, selVars, drop=FALSE], verbose=TRUE)$class 

  # compute accuracy
  acc = mean(Ytest == ynew)
  
  return(acc)  
}

K = 10 # number of folds
B = 20 # number of repetitions


# LDA using the top 100 features ranked by CAT scores
# (combined across groups using "entropy" for overall ranking)
set.seed(12345)
cv.lda100 = crossval(predfun, Xtrain, Ytrain, K=K, B=B, numVars=100, 
              diagonal=FALSE)
cv.lda100$stat
# 1


### comparison of LDA / DDA and "entropy" and "max" ranking.scores

# LDA using the top 10 features ranked by CAT scores
# (combined across groups using "entropy" for overall ranking)
set.seed(12345)
cv.lda10 = crossval(predfun, Xtrain, Ytrain, K=K, B=B, numVars=10, 
             diagonal=FALSE)
cv.lda10$stat
# 0.9909762

# DDA using the top 10 features ranked by t scores
# (combined across groups using "entropy" for overall ranking)
set.seed(12345)
cv.dda10 = crossval(predfun, Xtrain, Ytrain, K=K, B=B, numVars=10, 
             diagonal=TRUE)
cv.dda10$stat
# 0.9643869

# DDA using the top 10 features ranked by t scores,
# (combined across groups using "max" for overall ranking, as in PAM)
set.seed(12345)
cv.dda10b = crossval(predfun, Xtrain, Ytrain, K=K, B=B, numVars=10, 
              diagonal=TRUE, ranking.score="max")
cv.dda10b$stat
# 0.9585595

