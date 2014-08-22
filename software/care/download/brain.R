# requires "care" R package version 1.1.1

library("care")

# load Lu et al. (2004) data set
data(lu2004)
x = lu2004$x
y = lu2004$y # age
dim(x) # 30 403

# regularization parameters and number of included predictors
reg = slm(x,y)$regularization
lambda = reg[1] # 0.1373293
lambda.var = reg[2] # 0.0245981
numpred =  c(seq(5, 45, by=10), seq(53, 403, by=25))
numpred

# ordering according to CAR scores 
car = carscore(x, y, lambda=lambda)
ocar = order(car^2, decreasing=TRUE)
car.predlist = make.predlist(ocar, numpred, name="CAR")
car.models = slm.models(x, y, car.predlist, lambda=lambda, lambda.var=lambda.var)

# ordering accoding to marginal correlations 
marg = carscore(x, y, lambda=lambda, diagonal=TRUE) # shrinking not actually reqired
omarg = order(marg^2, decreasing=TRUE)
marg.predlist = make.predlist(omarg, numpred, name="MARG")
marg.models = slm.models(x, y, marg.predlist, lambda=lambda, lambda.var=lambda.var)

# make plot
ylim = range( c(marg.models$R2, car.models$R2) )
plot(car.models$numpred, car.models$R2, type="b", ylim=ylim,
  xlab="number of included predictors",
  ylab="model fit (R2)",
  main="CAR and Marginal Correlation Models for Brain Data")
points(marg.models$numpred, marg.models$R2, col=2, type="b")
legend("bottomright", c("CAR Score", "Marginal Correlation"), col=c(1,2), lty=c(1,1) )



### estimate prediction error by cross-validation

library("crossval")

# standardize data (as in Zuber and Strimmer 2011)
xs = scale(x)
ys = scale(y)

K=5  # number of folds
B=100 # number of repetions


# Rank by CAR scores, fit and predict using a specified number of predictors
predfun = function(Xtrain, Ytrain, Xtest, Ytest, numVars)
{  
  # rank the variables according to squared CAR scores
  car = carscore(Xtrain, Ytrain, verbose=FALSE)
  ocar = order(car^2, decreasing=TRUE)
  selVars = ocar[1:numVars]

  # fit and predict
  slm.fit = slm(Xtrain[, selVars, drop=FALSE], Ytrain, verbose=FALSE)
  Ynew = predict(slm.fit, Xtest[, selVars, drop=FALSE], verbose=FALSE)

  # compute squared error risk
  mse = mean( (Ynew - Ytest)^2)  

  return(mse)  
}


### Table 9 in Zuber and Strimmer (2011)
set.seed(12345)
cvp = crossval(predfun, xs, ys, K=K, B=B, numVars = 36, verbose=FALSE)
c(cvp$stat, cvp$stat.se) # 0.3441316  0.007449175
set.seed(12345)
cvp = crossval(predfun, xs, ys, K=K, B=B, numVars = 60, verbose=FALSE)
c(cvp$stat, cvp$stat.se) # 0.3085344  0.006405896
set.seed(12345)
cvp = crossval(predfun, xs, ys, K=K, B=B, numVars = 85, verbose=FALSE)
c(cvp$stat, cvp$stat.se) # 0.297824   0.006178467 


### Figure 3 in Zuber and Strimmer (2011)
numpred = c(seq(10, 200, 10), 403) # number of predictors
set.seed(12345)
cvsim = lapply(numpred, 
  function(i)
  {
    cat("Number of predictors:", i, "\n")
    cvp = crossval(predfun, xs, ys, K=K, B=B, numVars = i, verbose=FALSE)
    return( cvp$stat.cv )
  }
)
boxplot(cvsim, names=numpred,
col=c(rep("grey", 4), rep("white", 16), "grey"),
 main="CAR Models for the Gene Expression Data", xlab="number of included predictors",
       ylab="estimated CV prediction error")

