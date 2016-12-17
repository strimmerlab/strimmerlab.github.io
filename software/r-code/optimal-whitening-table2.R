# 17 December 2016

# R code to reproduce results (Table 2) from:
# A. Kessy, A. Lewin, and K. Strimmer. Optimal whitening and decorrelation.
# The American Statistician, to appear.
# http://arxiv.org/abs/1512.00809


# create whitening matrix W from given covariance matrix Sigma
whiteningMatrix = function(Sigma, 
  method=c("ZCA", "PCA", "Cholesky", "ZCA-cor", "PCA-cor"))
{
  method=match.arg(method)

  if(method=="ZCA" | method=="PCA")
  {
    eSigma = eigen(Sigma, symmetric=TRUE) 
    U = eSigma$vectors
    lambda = eSigma$values
    
    # fix sign ambiguity in eigenvectors by making U positive diagonal
    U = sweep(U, 2, sign(diag(U)), "*")

    W = diag(1/sqrt(lambda)) %*% t(U)
    if (method=="ZCA") W = U %*% W
  }

  if(method=="Cholesky")
  {
     W = chol(solve(Sigma))
  }

  if(method=="ZCA-cor" | method=="PCA-cor")
  {
    v = diag(Sigma)
    R = cov2cor(Sigma)
    eR = eigen(R, symmetric=TRUE) 
    G = eR$vectors
    theta = eR$values

    # fix sign ambiguity in eigenvectors by making G positive diagonal
    G = sweep(G, 2, sign(diag(G)), "*")

    W = diag(1/sqrt(theta)) %*% t(G) %*% diag(1/sqrt(v))
    if (method=="ZCA-cor") W = G %*% W
  }

  return (W)
}

# compute the values for each column in Table 2
# Phi: cross-covariance matrix; Psi: cross-correlation matrix
columnTable2 = function(Phi, Psi)
{
   out = c( diag(Psi),      # cor(zi, xi)
            sum(diag(Phi)), # trace(Phi)
            sum(diag(Psi)), # trace(Psi)
            max(rowSums(Phi^2)), 
            max(rowSums(Psi^2)) )
  return( round(out, 4))
}

# example data set
# E. Anderson. 1935.  The irises of the Gaspe Peninsula.
# Bull. Am. Iris Soc. 59: 2--5
data("iris")
X = as.matrix(iris[,1:4])
d = ncol(X) # 4
n = nrow(X) # 150
colnames(X) # "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"

# estimate covariance
S = cov(X)


# the five natural whitening transformations discussed in the paper
# applied to the iris example data set

# ZCA-Mahalanobis whitening
W.ZCA = whiteningMatrix(S, method="ZCA")
Z.ZCA = tcrossprod(X, W.ZCA) # whitened data
zapsmall(cov(Z.ZCA))
Phi.ZCA = cov(Z.ZCA, X) # cross-covariances
Psi.ZCA = cor(Z.ZCA, X) # cross-correlations

# PCA whitening
W.PCA = whiteningMatrix(S, method="PCA")
Z.PCA = tcrossprod(X, W.PCA) # whitened data
zapsmall(cov(Z.PCA))
Phi.PCA = cov(Z.PCA, X) # cross-covariances
Psi.PCA = cor(Z.PCA, X) # cross-correlations

# Cholesky whitening
W.Chol = whiteningMatrix(S, method="Cholesky")
Z.Chol = tcrossprod(X, W.Chol) # whitened data
zapsmall(cov(Z.Chol))
Phi.Chol = cov(Z.Chol, X) # cross-covariances
Psi.Chol = cor(Z.Chol, X) # cross-correlations

# ZCA-cor whitening
W.ZCAcor = whiteningMatrix(S, method="ZCA-cor")
Z.ZCAcor = tcrossprod(X, W.ZCAcor) # whitened data
zapsmall(cov(Z.ZCAcor))
Phi.ZCAcor = cov(Z.ZCAcor, X) # cross-covariances
Psi.ZCAcor = cor(Z.ZCAcor, X) # cross-correlations

# PCA-cor whitening
W.PCAcor = whiteningMatrix(S, method="PCA-cor")
Z.PCAcor = tcrossprod(X, W.PCAcor) # whitened data
zapsmall(cov(Z.PCAcor))
Phi.PCAcor = cov(Z.PCAcor, X) # cross-covariances
Psi.PCAcor = cor(Z.PCAcor, X) # cross-correlations


# create Table 2
tab2 = cbind( 
        columnTable2(Phi.ZCA,    Psi.ZCA),
        columnTable2(Phi.PCA,    Psi.PCA),
        columnTable2(Phi.Chol,   Psi.Chol),
        columnTable2(Phi.ZCAcor, Psi.ZCAcor),
        columnTable2(Phi.PCAcor, Psi.PCAcor) )
colnames(tab2) = c("ZCA", "PCA", "Cholesky", "ZCA-cor", "PCA-cor")
rownames(tab2) = NULL
tab2

#        ZCA    PCA Cholesky ZCA-cor PCA-cor
#[1,] 0.7137 0.8974   0.3760  0.8082  0.8902
#[2,] 0.9018 0.8252   0.8871  0.9640  0.8827
#[3,] 0.8843 0.0121   0.2700  0.6763  0.0544
#[4,] 0.5743 0.1526   1.0000  0.7429  0.0754
#[5,] 2.9829 1.2405   1.9368  2.8495  1.2754
#[6,] 3.0742 1.8874   2.5331  3.1914  1.9027
#[7,] 3.1163 4.2282   3.9544  1.7437  4.1885
#[8,] 1.9817 2.8943   2.7302  1.0000  2.9185

