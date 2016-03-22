# 20 March 2016 Korbinian Strimmer

# test example for whiten() function

#####

# load whitening function
source("whiten.R")

#####

# quick test of whitening

data("iris")
X = as.matrix(iris[,1:4])

# whitened version
Xw = whiten(X)

cov(X) # covariance of data set
zapsmall( cov(Xw) ) # covariance of whitened data set 


#####

# optimal whitening and CAR score (= correlation-adjusted marginal correlation)

library("care")
data(efron2004)
X = efron2004$x
Y = efron2004$y

# empirical car score
carscore(X, Y, lambda=0)
#        age         sex         bmi          bp          s1          s2 
# 0.06095381 -0.07992232  0.41283699  0.28089065  0.00861244  0.03022669 
#         s3          s4          s5          s6 
#-0.20727493  0.19318412  0.38446853  0.17095516 


# for comparison: marginal correlations
cor(Y, X)  # carscore(X, Y, lambda=0, diagonal=TRUE)
#       age        sex        bmi         bp         s1         s2         s3 
# 0.1878888  0.0430620  0.5864501  0.4414838  0.2120225  0.1740536 -0.3947893 
#        s4         s5         s6 
# 0.4304529  0.5658834  0.3824835 

# now we whiten data set
Xw = whiten(X)
Y = scale(Y) # acually, Y is already rescaled
cor(Y, Xw)
#            age         sex      bmi        bp         s1         s2         s3
#[1,] 0.06095381 -0.07992232 0.412837 0.2808907 0.00861244 0.03022669 -0.2072749
#            s4        s5        s6
#[1,] 0.1931841 0.3844685 0.1709552

# Result: Using CAR scores is same as optimal whitening then using marginal correlation

#####

# connection with CAT score  (correlation-adjusted t-score)

library("sda")
# shortcut for empirical cat score (no shrinkage)
catscore.empirical = function(X, L, diagonal=FALSE) 
{
  out = catscore(X, L, lambda=0, lambda.var=0, lambda.freqs=0, diagonal=diagonal, verbose=FALSE)
  attr(out, "lambda") = NULL
  attr(out, "lambda.freqs") = NULL
  attr(out, "lambda.var") = NULL
  attr(out, "lambda.estimated") = NULL
  attr(out, "lambda.freqs.estimated") = NULL
  attr(out, "lambda.var.estimated") = NULL
  attr(out, "class") = NULL
  attr(out, "freqs") = NULL
  return(out)
}

data("iris")
X = as.matrix(iris[,1:4])

# for illustration we only use two groups: setosa vs. other
L = factor( ifelse(iris$Species=="setosa", "setosa", "other") )

# compute empirical CAT score
catscore.empirical(X, L)
#              cat.other cat.setosa
#Sepal.Length   2.818615  -2.818615
#Sepal.Width  -18.167878  18.167878
#Petal.Length  29.314490 -29.314490
#Petal.Width   16.225132 -16.225132



# for comparison: compute standard empirical t-score
catscore.empirical(X, L, diagonal=TRUE)
#               t.other   t.setosa
#Sepal.Length 12.528237 -12.528237
#Sepal.Width  -9.204068   9.204068
#Petal.Length 29.130840 -29.130840
#Petal.Width  23.410974 -23.410974

# this is same as using built-in R t.test
t.test(X[,1] ~ L, var.equal=TRUE)$statistic # 12.52824
t.test(X[,2] ~ L, var.equal=TRUE)$statistic # -9.204068
t.test(X[,3] ~ L, var.equal=TRUE)$statistic # 29.13084
t.test(X[,4] ~ L, var.equal=TRUE)$statistic # 23.41097

# then whiten data set taking into account groups
Xw = whiten(X, L)

# t-score from whitened data
catscore.empirical(Xw, L, diagonal=TRUE)
#                t.other   t.setosa
#Sepal.Length   2.818615  -2.818615
#Sepal.Width  -18.167878  18.167878
#Petal.Length  29.314490 -29.314490
#Petal.Width   16.225132 -16.225132


# Result: Using CAT scores is same as optimal whitening followed by
# using standard t-score


