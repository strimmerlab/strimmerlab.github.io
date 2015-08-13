#Lösung des sechsten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#16. November 2010
########################################################################
#Aufgabe 1
########################################################################

#(a) 
dtest <- rnorm(1000, mean = 0, sd = 1) 

#(b) 
dtest <- dtest[which(abs(dtest) < 1 )]
length(dtest)

#(c) 
hist(dtest, freq=FALSE, xlim = c(-3,3), breaks=19)

#(d) 
lines(density(dtest), col="green")


####################
#Man sieht, dass der Kerndichteschaetzer den abgeschnittenen Bereich
#groesser |1| nicht erkennt.
#Obwohl in diesem Bereich keine Werte liegen, ist er deutlich groesser Null.
#Dies ist darauf zurückzuführen, dass der Kerndichteschaetzer 
#auf einer (besonders gewichteten) Mittelwertsbildung basiert.  



########################################################################
#Aufgabe 2
########################################################################

#(a)

find.max<-function(matrix){
matrix<-AirPassengers
n<-nrow(matrix)
m<-ncol(matrix)
max.matrix<-max(matrix)
max.row<-0
max.col<-0
for(i in 1:n){
 for(j in 1:m){
  if(matrix[i,j]==max.matrix){
   max.row<-i
   max.col<-j
   }  
  }
 }
return(list(Zeile=max.row, Spalte=max.col, Maximum=max.matrix))
}



#(b)


find.ext<-function(matrix, option="max"){

n<-nrow(matrix)
m<-ncol(matrix)
if(option=="max"){
ext.matrix<-max(matrix)
}
if(option=="min"){
ext.matrix<-min(matrix)
}

max.row<-0
max.col<-0
for(i in 1:n){
 for(j in 1:m){
  if(matrix[i,j]==ext.matrix){
   max.row<-i
   max.col<-j
   }  
  }
 }

return(list(Zeile=max.row, Spalte=max.col, Extremum=ext.matrix))
}


#(c)

matrix<-matrix(rnorm(100*100), ncol=100, nrow=100)
dim(matrix)

#Test von find.max

result<-find.max(matrix)
matrix[result$Zeile,result$Spalte]==result$Maximum

#Test von find.ext

result<-find.ext(matrix, option="min")
matrix[result$Zeile,result$Spalte]==result$Extremum


########################################################################
#Aufgabe 3
########################################################################


 U1 <- runif(10000)
 U2 <- runif(10000)
 X <- U1 + U2
 Y <- U1 - U2
 plot(Y,X)
 cor(X,Y)

#(a)
#=> linear Unabhängig, da Korrelation sehr klein

#(b)
# Dichte von X: |x-1| , x \in [0,2]
# Dichte von Y: |y+1| , y \in [-1,1]

#=> klare Abhängigkeit erkennbar es ist stets X <= Y


#(c)
cor(U1, U2)
#=> linear Unabhängig, da Korrelation sehr klein

#(d)
plot(U1, U2)

# => keine Abhängigkeit erkennbar

