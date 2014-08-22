#Lösung des achten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#1. Dezember 2011
########################################################################


########################################################################
#Aufgabe 1
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
#Aufgabe 2
########################################################################


 U1 <- runif(10000)
 U2 <- runif(10000)
 X <- U1 + U2
 Y <- U1 - U2
 plot(Y,X)
 cor(X,Y)

#(a)
#=> linear Unabhaengig, da Korrelation sehr klein

#(b)
# Dichte von X: |x-1| , x \in [0,2]
# Dichte von Y: |y+1| , y \in [-1,1]

#=> klare Abhaengigkeit erkennbar es ist stets X <= Y


#(c)
cor(U1, U2)
#=> linear Unabhaengig, da Korrelation sehr klein

#(d)
plot(U1, U2)

# => keine Abhaengigkeit erkennbar





########################################################################
#Aufgabe 3 Gesetz der großen Zahlen
########################################################################


#(a)
test <- rnorm(100, mean = 0, sd = 1)

#(b)

simNV <- function(mu = 0, sigma = 1){
vars = rep(1,6)
mus = rep(0,6)

for(i in 1:6){	
tmp <- rnorm(10^i, mean = mu, sd = sigma)
vars[i]=var(tmp)
mus[i]=mean(tmp)
}

res <- data.frame(mu = mus, sigma2 = vars)
return(res)

}

#(c) 
ew <- (simNV(mu = 100, sigma = 2))$mu
plot(ew, pch = 17)

#(d) 
par(mfrow=c(5,2))
for(i in 1:10){ 
ew <- (simNV(mu = 100, sigma = 2))$mu
plot(ew, pch = 17, ylim = c(99.0, 101.0))
lines(c(1,6),c(100,100), col=6, lty=3)
}


###########################
#Es ist zu sehen, dass bei geringer Fallzahl der beobachtete Erwartungswert
#starke Abweichungen vom wahren Erwartungswert hat.
#Je groesser die Fallzahl, umso genauer der beobachtete Erwartungswert.



########################################################################
#Aufgabe 4
########################################################################



#(a)

data<-rnorm(100, mean=1)
myttest<-t.test(data, alternative="two.sided")
myttest$p.value


#b

testFunction<-function(mu,n, sqvar=1){
data<-rnorm(n,mean=mu,sd=sqvar)
myttest<-t.test(data, alternative="two.sided")
myttest$p.value
}


#c

Stichproben<-c(5,10,100,1000,10000, 100000)
pWerte<-rep(0,length(Stichproben))

for(i in 1:length(Stichproben)){
pWerte[i]<-testFunction(mu=1,n=Stichproben[i],sqvar=1)
}

plot( pWerte, type="l", xaxt="n", xlab="Stichprobengroesse")
axis(side = 1,  at = seq(1:length(Stichproben)), labels = Stichproben )


#d 

Standardab<-c(0.001,0.01,0.1,1,10,100, 1000)
pWerte<-matrix(nrow=length(Stichproben), ncol=length(Standardab))

dim(pWerte)


	for(i in 1:length(Stichproben)){
		for(j in 1:length(Standardab)){
		pWerte[i,j]<-testFunction(mu=1,n=Stichproben[i],sqvar=Standardab[j])
		}
	}
dev.new(width = 15, height = 8)
contour(x= seq(1:length(Stichproben)), y=  seq(1:length(Standardab)), z = pWerte, xaxt="n", yaxt="n",
levels = c(seq(0,0.1, by = 0.05), seq(0.1,1, by = 0.1)), labcex = 2, method = "edge", xlab="Stichprobengroesse", ylab = "Standardabweichung")
axis(side = 1,  at = seq(1:length(Stichproben)), labels = Stichproben )
axis(side = 2,  at = seq(1:length(Standardab)), labels = Standardab )




image(x= seq(1:length(Stichproben)), y=  seq(1:length(Standardab)), z = pWerte, col=terrain.colors(4),  xaxt="n", yaxt="n",xlab="Stichprobengroesse", ylab = "Standardabweichung")
axis(side = 1,  at = seq(1:length(Stichproben)), labels = Stichproben )
axis(side = 2,  at = seq(1:length(Standardab)), labels = Standardab )



