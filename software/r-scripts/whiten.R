# 20 March 2016 Korbinian Strimmer


# Whiten data set using empirical covariance estimator
# following the optimal whitening procedure in Kessy et al. 2015
# http://arxiv.org/abs/1512.00809
# if samples have class labels (L) then the pooled covariance estimator is used
# requires "sda" R package computing pooled covariance estimator

whiten = function(X, L, center=TRUE)
{
    require("sda")
   
    if ( missing(L) ) L = as.factor(rep("single class", nrow(X)) )

    tmp = centroids(X, L, lambda.var=0, lambda.freqs=0, 
          var.groups=FALSE, centered.data=TRUE, verbose=FALSE)
    sc = sqrt(tmp$variances[,1]) # sqrt of pooled variance

    # CAT-CAR whitening using empirical (pooled) variances and (pooled) correlation
    X.whitened = t( crossprod.powcor.shrink(tmp$centered.data, t(sweep(X, 2, sc, "/")), 
                     alpha=-1/2, lambda=0, verbose=FALSE) )

    if(center) X.whitened = scale(X.whitened, scale=FALSE)

    return(X.whitened)
}

