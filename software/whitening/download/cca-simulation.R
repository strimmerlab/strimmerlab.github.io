# 3 July 2018

library("mnormt")
library("whitening")

# estimate proportion of correct signs of canonical correlations

# n: sample size
# a: correlation (magnitude)
# b: repeats
simulateCCA = function(n, a, b)
{
  p=50 
  q=10

  # setup mean and covariance
  mu=rep(0,p+q)
  rho = c(c(a, -a),c(a, -a),c(a, -a),c(a, -a),c(a, -a))
  Sigma = diag(1, p+q)
  for(i in 1:q)
  {
    Sigma[i, p+i] = rho[i]
    Sigma[p+1, i] = rho[i]
  }

  result = integer(b)
  for (i in 1:b)
  {
    if( (i-1) %% 10 == 0) cat("Iteration #", i, "\n")
  
    # simulate data
    Z = rmnorm(n, mu, Sigma)
    X = Z[,1:p]
    Y = Z[,(p+1):(p+q)]

    # compute cca
    scca.out = scca(X, Y, verbose=FALSE)
    result[i] = sum( sign(scca.out$lambda) == sign(rho)) # number of correct signs
  }

  return(result/q)
}


n.vec=c(20, 30, 50, 100, 200, 500)
a.vec=c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3)

res = matrix(0, nrow=length(n.vec), ncol=length(a.vec))

set.seed(123)

for (k in 1:length(a.vec))
{
  for (i in 1:length(n.vec))
  {
    cat("a=", a.vec[k], ", n=", n.vec[i], "\n")

    out = simulateCCA(n=n.vec[i], a=a.vec[k], b=500)
    res[i,k] = mean(out)
  }
}

save(res, file="res.rda")

####

load("res.rda")

pdf(width=8, file="signsim.pdf")

plot(n.vec, res[,1]*100, type="b", 
main="Sign of Canonical Correlations",
ylim=c(50, 100), lwd=2,
log="x", xlab="Sample Size", ylab="Correctly Identified (%)")

nicecol=c(1,2,3,4,5,6,8)
for (k in 2:length(a.vec))
{
  lines(n.vec, res[,k]*100, col=nicecol[k], lty=k, lwd=2)
  points(n.vec, res[,k]*100, col=nicecol[k])
}

legend("bottomright", cex=1.4,
c(expression(paste(lambda, " = ", 0.9)),
  expression(paste(lambda, " = ", 0.8)),
  expression(paste(lambda, " = ", 0.7)),
  expression(paste(lambda, " = ", 0.6)),
  expression(paste(lambda, " = ", 0.5)),
  expression(paste(lambda, " = ", 0.4)),
  expression(paste(lambda, " = ", 0.3)) ), 
lty=1:7, col=nicecol, text.col=nicecol, lwd=rep(2, 7))

dev.off()


