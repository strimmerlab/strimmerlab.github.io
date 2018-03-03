# /*
# This is an R script containing R markdown comments.  It can be run as is in R.
# To generate a document containing the formatted R code, R output and markdown 
# click the "Compile Report" button in R Studio, or run the command
# rmarkdown::render() - see http://rmarkdown.rstudio.com/r_notebook_format.html
# */


#' ---
#' title: "Table 2 of Kessy, Lewin, and Strimmer (2018)"
#' output: pdf_document
#' author: ""
#' date: Requires "whitening" version 1.0.0 (March 2018) or later
#' ---

#' This script reproduces the results of Table 2 in:
#' A. Kessy, A. Lewin, and K. Strimmer. 2018. 
#' *Optimal whitening and decorrelation.*
#' The American Statistician
#' https://doi.org/10.1080/00031305.2016.1277159

#'
#' # Load "whitening" package and example data

library("whitening") # available on CRAN


#' Example data set:
#' E. Anderson. 1935.  The irises of the Gaspe Peninsula.
#' Bull. Am. Iris Soc. 59: 2--5
data("iris")
X = as.matrix(iris[,1:4])
d = ncol(X) # 4
n = nrow(X) # 150
colnames(X) # "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"


#'
#' # Five natural whitening transformations applied to the iris example data set

#' ZCA-Mahalanobis whitening:
Z.ZCA = whiten(X, method="ZCA") # whitened data
zapsmall(cov(Z.ZCA))
Phi.ZCA = cov(Z.ZCA, X) # cross-covariances
Psi.ZCA = cor(Z.ZCA, X) # cross-correlations

#' PCA whitening:
Z.PCA = whiten(X, method="PCA") # whitened data
zapsmall(cov(Z.PCA))
Phi.PCA = cov(Z.PCA, X) # cross-covariances
Psi.PCA = cor(Z.PCA, X) # cross-correlations

#' Cholesky whitening:
Z.Chol = whiten(X, method="Cholesky") # whitened data
zapsmall(cov(Z.Chol))
Phi.Chol = cov(Z.Chol, X) # cross-covariances
Psi.Chol = cor(Z.Chol, X) # cross-correlations

#' ZCA-cor whitening:
Z.ZCAcor = whiten(X, method="ZCA-cor") # whitened data
zapsmall(cov(Z.ZCAcor))
Phi.ZCAcor = cov(Z.ZCAcor, X) # cross-covariances
Psi.ZCAcor = cor(Z.ZCAcor, X) # cross-correlations

#' PCA-cor whitening:
Z.PCAcor = whiten(X, method="PCA-cor") # whitened data
zapsmall(cov(Z.PCAcor))
Phi.PCAcor = cov(Z.PCAcor, X) # cross-covariances
Psi.PCAcor = cor(Z.PCAcor, X) # cross-correlations


#
#' # Create Table 2

#' Helper function to compute entries of a column in Table 2 
#'(Phi: cross-covariance matrix; Psi: cross-correlation matrix):
columnTable2 = function(Phi, Psi)
{
   out = c( diag(Psi),      # cor(zi, xi)
            sum(diag(Phi)), # trace(Phi)
            sum(diag(Psi)), # trace(Psi)
            max(rowSums(Phi^2)), 
            max(rowSums(Psi^2)) )
  return( round(out, 4))
}

#' Compute Table 2:
tab2 = cbind( 
        columnTable2(Phi.ZCA,    Psi.ZCA),
        columnTable2(Phi.PCA,    Psi.PCA),
        columnTable2(Phi.Chol,   Psi.Chol),
        columnTable2(Phi.ZCAcor, Psi.ZCAcor),
        columnTable2(Phi.PCAcor, Psi.PCAcor) )
colnames(tab2) = c("ZCA", "PCA", "Cholesky", "ZCA-cor", "PCA-cor")
rownames(tab2) = NULL
tab2

# /*
#'        ZCA    PCA Cholesky ZCA-cor PCA-cor
# [1,] 0.7137 0.8974   0.3760  0.8082  0.8902
# [2,] 0.9018 0.8252   0.8871  0.9640  0.8827
# [3,] 0.8843 0.0121   0.2700  0.6763  0.0544
# [4,] 0.5743 0.1526   1.0000  0.7429  0.0754
# [5,] 2.9829 1.2405   1.9368  2.8495  1.2754
# [6,] 3.0742 1.8874   2.5331  3.1914  1.9027
# [7,] 3.1163 4.2282   3.9544  1.7437  4.1885
# [8,] 1.9817 2.8943   2.7302  1.0000  2.9185
# */
