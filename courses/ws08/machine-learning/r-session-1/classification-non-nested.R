# orginal script by Florian Reithinger / LMU

set.seed(345);

#pdf("plot1.pdf",height=15,width=15);
par(mfrow=c(3,4));

library(mvtnorm);
library(MASS);



probs<-c(0.2,0.2,0.5,0.1);
ind<-rmultinom(1000, size = 1, prob=probs)

mu1<-c(-3,-3);
mu2<-c(4,4);
mu3<-c(3,-3);
mu4<-c(-3,3);

S1<-matrix(nrow=2,ncol=2);
S1[,1]<-c(1,1);
S1[,2]<-c(1,2);

S2<-S1;
S2[,1]<-c(2,1);

S3<-S2;
S3[,2]<-c(1,1);

S4<-S1;
S4[,1]<-c(4,-3);
S4[,2]<-c(-3,5);


x1<-rmvnorm(1000,mu1,S1);
x2<-rmvnorm(1000,mu2,S2);
x3<-rmvnorm(1000,mu3,S3);
x4<-rmvnorm(1000,mu4,S4);

xlist<-list(x1,x2,x3,x4);

indnumber<-t(ind)%*%1:4
table(indnumber);
estprobs<-table(indnumber)/1000;

des<-x1;
for(i in 1:1000){
des[i,]<- xlist[[indnumber[i]]][i,];
}




#plot(des[,1],des[,2]);

basiscolors<-c("blue","green","red","orange");

colors<-basiscolors[indnumber]

plot(des[,1],des[,2],col=colors);
title("original data")


x<-seq(-5,5,length=100);
xvech<-rep(x,100);
yvech<-rep(x,each=100);
newdes<-cbind(xvech,yvech);

v1<-des[,1];
v2<-des[,2];

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

#plot(da.ksvm);


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
nam<-c("a","b","c","d")
namind<-nam[indnumber];


rf<-nnet(as.factor(namind)~v1+v2,size=2);
pred.rf<-predict(rf,newdes);

maxrf<-apply(pred.rf,1,max)

nnindmatrix<-matrix(nrow=10000,ncol=4);
nnindmatrix[,1]<-as.numeric(pred.rf[,1]==maxrf)
nnindmatrix[,2]<-as.numeric(pred.rf[,2]==maxrf)
nnindmatrix[,3]<-as.numeric(pred.rf[,3]==maxrf)
nnindmatrix[,4]<-as.numeric(pred.rf[,4]==maxrf)

nnind<-nnindmatrix%*%(1:4)


newcol<-basiscolors[nnind];
plot(newdes[,1],newdes[,2],col=newcol);
title("Neuronal Net")



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

## unequal variances
library(nnet)

ld.multinom<-multinom(indnumber ~ v1 + v2 );

newcol<-basiscolors[predict(ld.multinom,newdes)]

plot(newdes[,1],newdes[,2],col=newcol);
title("Multinomial Logit (unequal var)")



## equal variances
ld.multinom<-multinom(indnumber ~ v1 + v2 +v1*v2 +v2*v2);

newcol<-basiscolors[predict(ld.multinom,newdes)]

plot(newdes[,1],newdes[,2],col=newcol);
title("Multinomial Logit (equal var)")

#dev.off();
