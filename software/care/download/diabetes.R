
# requires "care" R package version 1.1.1

library("care")

# diabetes data
data(efron2004)
x = efron2004$x
y = efron2004$y
xnames = colnames(x)

#####

# ordering suggested by CAR scores
car = carscore(x, y, lambda=0)
ocar = order(car^2, decreasing=TRUE)
xnames[ocar]

# plot regression coefficients for all possible CAR models
p = ncol(x)
car.predlist = make.predlist(ocar, numpred = 1:p, name="CAR")
cm = slm.models(x, y, car.predlist, lambda=0, lambda.var=0)
bmat= cm$coefficients[,-1]
bmat

plot(1:p, bmat[,1], type="l", 
  ylab="estimated regression coefficients", 
  xlab="number of included predictors", 
  main="CAR Regression Models for Diabetes Data", 
  xlim=c(1,p+1), ylim=c(min(bmat), max(bmat)))

for (i in 2:p) lines(1:p, bmat[,i], col=i, lty=i)
for (i in 1:p) points(1:p, bmat[,i], col=i)
for (i in 1:p) text(p+0.5, bmat[p,i], xnames[i])




### estimate prediction error by cross-validation

library("crossval")

K=10  # number of folds
B=50 # number of repetitions


# Rank by CAR scores, fit and predict using a specified number of predictors
predfun = function(Xtrain, Ytrain, Xtest, Ytest, numVars)
{  
  # rank the variables according to squared CAR scores
  car = carscore(Xtrain, Ytrain, verbose=FALSE, lambda=0)
  ocar = order(car^2, decreasing=TRUE)
  selVars = ocar[1:numVars]

  # fit and predict
  slm.fit = slm(Xtrain[, selVars, drop=FALSE], Ytrain, verbose=FALSE, 
       lambda=0, lambda.var=0)
  Ynew = predict(slm.fit, Xtest[, selVars, drop=FALSE], verbose=FALSE)

  # compute squared error risk
  mse = mean( (Ynew - Ytest)^2)  

  return(mse)  
}


numpred = 1:10 # number of predictors
set.seed(12345)
cvsim = lapply(numpred, 
  function(i)
  {
    cat("Number of predictors:", i, "\n")
    cvp = crossval(predfun, x, y, K=K, B=B, numVars = i, verbose=FALSE)
    return( cvp$stat.cv )
  }
)
boxplot(cvsim, names=numpred,
col=c(rep("grey", 2), rep("white", 8)),
 main="CAR Models for the Diabetes Data", xlab="number of included predictors",
       ylab="estimated CV prediction error")

# After including top 3 predictors ("bmi", "s5", "bp") no further reduction of MSE is seen




