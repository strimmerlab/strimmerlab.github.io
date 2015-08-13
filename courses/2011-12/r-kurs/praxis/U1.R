#Loesung des ersten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#13 Oktober 2011
########################################################################



#Aufgabe 2


#b
########################################################################
a <- 3
b <- 4.5

#c
########################################################################

is.numeric(a)
is.character(b)

#d
########################################################################

A <- matrix(seq(1,9), nrow = 3)
A[3,3] <- 10
#A<-matrix(c(1,2,3,4,5,6,7,8,10),nrow=3)

B <- matrix(seq(1,9), nrow = 3, byrow = TRUE)
B[3,3] <- 10
#B<-matrix(c(1,2,3,4,5,6,7,8,10),nrow=3, byrow=TRUE)


y<-matrix(c(1,2,3), nrow=3)

#e
########################################################################
a^2 + 1 /b
a*A
A%*%B

det(A)
#Ist die Determinante einer Matrix gleich Null, so ist die Matrix singulär
#und kann nicht invertiert werden 
solve(A)
t(A)



#f
########################################################################

A[2,3]
B[3,2]

#g
########################################################################

# elementweise:
#A[1,]*B[,2]

# a^T * b
A[1,]%*%B[,2]

# b^T*a
ez.A<-as.matrix(A[1,])
zs.B<-as.matrix(t(B[,2]))

ez.A%*%zs.B

#h
########################################################################

beta<-solve(t(A)%*%A)%*%t(A)%*%y

#Alternativ über das lineare Modell in der Funktion lm()

########################################################################
########################################################################



#Aufgabe 2


x<-seq(from=-2, to=2, by=0.2)
length(x)
x
standNV<-dnorm(x, mean=0, sd=1)
# oder: standNV<-dnorm(x)
length(standNV)
standNV

#Visualisierung mit dem plot() Befehl
#plot(x,standNV, type="l")


########################################################################
########################################################################



#Aufgabe 3

#a
########################################################################

pat<-read.csv("Daten/Patienten.csv")
dim(pat)

#b
########################################################################

is.data.frame(pat)
summary(pat)

#c
########################################################################

head(pat)
pat

#d
########################################################################

attach(pat)
summary(Gewicht)
#Es besteht ein fehlender Wert, aufgrund der geringen Größe des Datensatzes
#ist es leicht zu entdecken, dass dies der zweite Patient ist
#Ansonsten mit der which()-Funktion arbeiten

#e
########################################################################

pat[2,2]<-mean(Gewicht, na.rm=TRUE)
pat

#Hier muss direkt auf den Datensatz zugegriffen werden

detach(pat)


########################################################################
########################################################################





