#Loesung des zweiten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#27 Oktober 2009
########################################################################



#Aufgabe 1

#a
########################################################################

A <- matrix(seq(1,9), nrow = 3)
A[3,3] <- 10
#A<-matrix(c(1,2,3,4,5,6,7,8,10),nrow=3)

B <- matrix(seq(1,9), nrow = 3, byrow = TRUE)
B[3,3] <- 10
#B<-matrix(c(1,2,3,4,5,6,7,8,10),nrow=3, byrow=TRUE)

B <- matrix(seq(1,9), nrow = 3, byrow = TRUE)

y<-matrix(c(1,2,3), nrow=3)

#b
########################################################################

det(A)
solve(A)
t(A)


#Ist die Determinante einer Matrix gleich Null, so ist die Matrix singulaer
#und kann nicht invertiert werden 


#c
########################################################################

A[1,]*B[,2]
A[1,]%*%B[,2]

ez.A<-as.matrix(A[1,])
zs.B<-as.matrix(t(B[,2]))

ez.A%*%zs.B

#d
########################################################################

beta<-solve(t(A)%*%A)%*%t(A)%*%y

#Alternativ über das lineare Modell in der Funktion lm()

########################################################################
########################################################################



#Aufgabe 2

#a
########################################################################

require(e1071)
help.search(sig)

#b
########################################################################


x<-seq(from=-2, to=2, by=0.2)
length(x)
x
y<-sigmoid(x)
length(y)

#plot(x,y, type="l")


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
#Es besteht ein fehlender Wert, aufgrund der geringen Groesse des Datensatzes
#ist es leicht zu entdecken, dass dies der zweite Patient ist
#Ansonsten mit der which()-Funktion arbeiten

#e
########################################################################

pat[2,2]<-mean(Gewicht, na.rm=TRUE)
pat

#Hier muss direkt auf den Datensatz zugegriffen werden

detach(pat)

########################################################################
#Hinweis: 
#Mit dem attach Befehl werden Kopien der Spalten des Datensatzes erzeugt.
#Aenderungen an diesen Kopien haben KEINE Auswirkung auf den eigentlichen 
#Datensatz


########################################################################
########################################################################



