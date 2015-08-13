#Lösung des zweiten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#21 Oktober 2010
########################################################################



#Aufgabe 1

#a
########################################################################

A <- matrix(seq(1,100), nrow = 4)
dim(A)

#[1]  4 25


B <- matrix(seq(1,100), ncol = 4)
dim(B)

#[1]  25 4

#b
########################################################################

meanA <- apply(X=A, MARGIN=1, FUN=mean)
meanA
varA <- apply(X=A, MARGIN=1, FUN=var)
varA


meanB <- apply(X=B, MARGIN=1, FUN=mean)
meanB
varB <- apply(X=B, MARGIN=1, FUN=var)
varB


#c
########################################################################

meanA <- apply(X=A, MARGIN=2, FUN=mean)
meanA
varA <- apply(X=A, MARGIN=2, FUN=var)
varA


meanB <- apply(X=B, MARGIN=2, FUN=mean)
meanB
varB <- apply(X=B, MARGIN=2, FUN=var)
varB


#d: Standardisieren der Spalten von A
########################################################################

mA <- rbind(meanA,meanA,meanA,meanA)
vA <- rbind(varA,varA,varA,varA)
stdA<- (A-mA)/sqrt(vA)

#Überprüfen ob die Spalten wirklich standardisiert sind

check1  <- apply(X=stdA, MARGIN=2, FUN=mean)
check2 <- apply(X=stdA, MARGIN=2, FUN=var)
check1
check2

########################################################################
########################################################################



