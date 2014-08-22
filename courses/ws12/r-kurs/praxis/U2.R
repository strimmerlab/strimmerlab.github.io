#Loesung des zweiten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#18 Oktober 2012
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



#Aufgabe 2

#a
########################################################################

oecd<-read.csv(file="Daten/oecd", header=TRUE, na.strings="NA")
#oecd<-read.csv(file="Daten/oecd", header=TRUE, sep="\t")

attach(oecd)
head(oecd)
names(oecd)

dim(oecd)


#b
########################################################################

sapply(oecd, mean, na.rm=TRUE)
sapply(oecd, var, na.rm=TRUE)

#c
########################################################################

 "Niederlande" %in% row.names(oecd)
which(row.names(oecd)%in%"Niederlande")
 "China" %in% row.names(oecd)


#d
########################################################################

Alkohol[which(Alkohol==max(Alkohol, na.rm=TRUE))]
row.names(oecd)[which(Alkohol==max(Alkohol, na.rm=TRUE))]

# wenn keine fehlenden Werte in den Daten sind analog zu: which.max/which.min



#e
########################################################################

Säuglsterblichkeit[which(Säuglsterblichkeit==min(Säuglsterblichkeit, na.rm=TRUE))]
row.names(oecd)[which(Säuglsterblichkeit==min(Säuglsterblichkeit, na.rm=TRUE))]


#f
########################################################################

Bewegung[which(Bewegung<mean(Bewegung, na.rm=TRUE))]
row.names(oecd)[which(Bewegung<mean(Bewegung, na.rm=TRUE))]

Einkommen[which(Einkommen<mean(Einkommen, na.rm=TRUE))]
row.names(oecd)[which(Einkommen<mean(Einkommen, na.rm=TRUE))]


#g
########################################################################

M<-mean(Bullying, na.rm=TRUE)
SA<-sd(Bullying, na.rm=TRUE)
row.names(oecd)[which( (Bullying > M + SA) )]


#h
########################################################################

ind<- order(Einkommen)	  
#Index der die Ränge der Länder bezüglich des Einkommen enthält
oecd2<-oecd[ind,]
#neuer nach ind angeordneter Datensatz


write.csv(oecd2, file="Daten/oecd2")

