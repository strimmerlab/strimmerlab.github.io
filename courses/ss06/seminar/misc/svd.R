##############################################
#
# "Showing that singular value decomposition
# just works fine and fast for 
# ridge regression with large p and small n"
# author: Simon R\"uckinger (2006)
#
##############################################



#design matrix X (2000 genes, 3 arrays), data vector y
X<-rbind(rnorm(2000),rnorm(2000),rnorm(2000))
y<-rnorm(3)



#Standard ridge regression (approx. 1 minute)
##############################################
ptm <- proc.time()

#Identity matrix dimension: 2000
I<-diag(2000)

#lambda
lambda<-0.5

#ridge regression solution
beta1<-solve((t(X)%*%X)+(lambda*I))%*%(t(X)%*%y)

proc.time() - ptm
###############################################



#Ridge regression using SVD (less than a second)
###############################################
ptm <- proc.time()

#Singular value decomposition of X
svd<-svd(X)
R<-svd$u%*%diag(svd$d)
V<-svd$v

#Identity matrix dimension: 3
I<-diag(3)

#svd solution
beta2<-V%*%solve(((t(R)%*%R)+(lambda*I)))%*%(t(R)%*%y)

proc.time() - ptm
###############################################



#For those, who don't want to believe: Comparison of beta1 and beta2
cbind(beta1,beta2)



