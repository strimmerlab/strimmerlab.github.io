# orginal script by Florian Reithinger / LMU

set.seed(123);

#pdf("plot2.pdf",height=15,width=15);
par(mfrow=c(3,4));

library(mvtnorm);
library(MASS);


mu<-c(0,0);

S<-matrix(nrow=2,ncol=2);
S[,1]<-c(1,1);
S[,2]<-c(1,2);


x<-rmvnorm(1000,mu,S);

dx<-apply(x,1,dmvnorm,mean=mu,sigma=S);

dx<-dx+rnorm(1000,sd=0.03);


indnumber<-as.numeric(dx>0.10)+1;



basiscolors<-c("blue","red");

colors<-basiscolors[indnumber]

plot(x[,1],x[,2],col=colors);
title("original data")


xseq<-seq(-5,5,length=100);
xvech<-rep(xseq,100);
yvech<-rep(xseq,each=100);
newdes<-cbind(xvech,yvech);

v1<-x[,1];
v2<-x[,2];

#########################
### lda
#########################

lda.des<-lda(indnumber~v1+v2);

newdes<-cbind(xvech,yvech);

newdes<-as.data.frame(newdes);
names(newdes)<-c("v1","v2");

newcol<-basiscolors[predict(lda.des,newdes)$class];

plot(newdes[,1],newdes[,2],col=newcol);

title("LDA")

#########################
###   qda
#########################


lda.des<-qda(indnumber~v1+v2);

newdes<-cbind(xvech,yvech);

newdes<-as.data.frame(newdes);
names(newdes)<-c("v1","v2");

newcol<-basiscolors[predict(lda.des,newdes)$class];

plot(newdes[,1],newdes[,2],col=newcol);

title("QDA")



#########################
###   support vector
#########################



library(e1071);

da.ksvm<-svm(indnumber~v1+v2,kernel="radial")

plot(da.ksvm);


pred.ksvm<-predict(da.ksvm,newdes);

pred.ksvm<-round(pred.ksvm);

newcol<-basiscolors[pred.ksvm];
plot(newdes[,1],newdes[,2],col=newcol);

title("SVM")



#tun.svm<-tune.svm(indnumber~v1+v2,gamma= 10^(-6:3),cost=10^(1:3));
#
#bestgamma<-tun.svm$best.parameters[[1]]
#bestC<-tun.svm$best.parameters[[2]]
#
#da.ksvm<-svm(indnumber~v1+v2,kernel="radial",cost=bestC,gamma=bestgamma);
#
#plot(da.ksvm);
#
#
#pred.ksvm<-predict(da.ksvm,newdes);
#
#pred.ksvm<-round(pred.ksvm);
#
#newcol<-basiscolors[pred.ksvm];
#plot(newdes[,1],newdes[,2],col=newcol);
#title("Tuned SVM")

#########################
###   cart
#########################
library(rpart);
rp<-rpart(indnumber~v1+v2);
pred.rp<-predict(rp,newdes);
newcol<-basiscolors[round(pred.rp)];
plot(newdes[,1],newdes[,2],col=newcol);

title("Cart")




#########################
###   randomforest
#########################

library(randomForest);
rf<-randomForest(as.factor(indnumber)~v1+v2,importance=T,proximity=T);
pred.rf<-predict(rf,newdes);
newcol<-basiscolors[as.numeric(pred.rf)];
plot(newdes[,1],newdes[,2],col=newcol);

title("Random Forest")


#########################
###   neuronal nets
#########################

library(nnet);
nam<-c("a","b")
namind<-nam[indnumber];


rf<-nnet(as.factor(namind)~v1+v2,size=2);
pred.rf<-predict(rf,newdes);

maxrf<-apply(pred.rf,1,max)

nnindmatrix<-matrix(nrow=10000,ncol=2);
nnindmatrix[,1]<-as.numeric(round(pred.rf[,1]))


nnind<-nnindmatrix[,1]


newcol<-basiscolors[nnind+1];
plot(newdes[,1],newdes[,2],col=newcol);
title("Neuronal Net,2 Neuronen")


#########################
###   neuronal nets
#########################

library(nnet);
nam<-c("a","b")
namind<-nam[indnumber];


rf<-nnet(as.factor(namind)~v1+v2,size=3);
pred.rf<-predict(rf,newdes);

maxrf<-apply(pred.rf,1,max)

nnindmatrix<-matrix(nrow=10000,ncol=2);
nnindmatrix[,1]<-as.numeric(round(pred.rf[,1]))


nnind<-nnindmatrix[,1]


newcol<-basiscolors[nnind+1];
plot(newdes[,1],newdes[,2],col=newcol);
title("Neuronal Net,3 Neuronen")


#########################
###   neuronal nets
#########################

library(nnet);
nam<-c("a","b")
namind<-nam[indnumber];


rf<-nnet(as.factor(namind)~v1+v2,size=1);
pred.rf<-predict(rf,newdes);

maxrf<-apply(pred.rf,1,max)

nnindmatrix<-matrix(nrow=10000,ncol=2);
nnindmatrix[,1]<-as.numeric(round(pred.rf[,1]))


nnind<-nnindmatrix[,1]


newcol<-basiscolors[nnind+1];
plot(newdes[,1],newdes[,2],col=newcol);
title("Neuronal Net,1 Neuron")



#########################
### fda
#########################

library(mda);

da.des<-fda(indnumber~v1+v2);

newcol<-basiscolors[predict(lda.des,newdes)$class];

plot(newdes[,1],newdes[,2],col=newcol);
title("Flexible Diskriminant Analysis")


#########################
### multinomial logit
#########################

## not equal variances
library(nnet)

ld.multinom<-multinom(indnumber ~ v1 + v2 );

newcol<-basiscolors[predict(ld.multinom,newdes)]

plot(newdes[,1],newdes[,2],col=newcol);
title("Multinomial Logit, Linear")



## equal variances
ld.multinom<-multinom(indnumber ~ v1 + v2 +v1*v2 +v2*v2);

newcol<-basiscolors[predict(ld.multinom,newdes)]

plot(newdes[,1],newdes[,2],col=newcol);
title("Multinomial Logit, Linear-Quadratisch")




#dev.off();












