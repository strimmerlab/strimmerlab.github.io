#Loesung des fuenften Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#11. November 2010
########################################################################

########################################################################
########################################################################



########################################################################
#Aufgabe 1
########################################################################


test <- treff(1000)
prop.table(table(test))

treff2 <- function(rep){
x <- rep(NA,rep)
for(i in 1:rep){	
x[i] <- rbinom(n=1, size = 1, prob = 0.70)
}
return(x)
}

#Simulation erfolgt über die Binomialverteilung mit der Wahrscheinlichkeit 0.7
#Mehr über Simulationen und Verteilungen am 25.11

########################################################################
#Aufgabe 2
########################################################################

geb <- function(n){
1-exp(lfactorial(365) - lfactorial(365-n) -n*log(365))
}

plot(1:100, geb(1:100), main = "Geburtstagsparadoxon", 
 xlab="Anzahl der Leute", ylab = "Wahrscheinlichkeit eines identischen  Geburtstages")


########################################################################
#Aufgabe 3
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

#100*100

#(a)

#matNV: Matrix mit 10 000 nv Zufallszahlen
matNV <- matrix(rnorm(10000, mean = 0, sd = 1), ncol=100, nrow=100)
dim(matNV)

#matC: Matrix mit 10 000 cauchy-verteilten Zufallszahlen
matC <- matrix(rcauchy(10000, location = 0, scale = 1), ncol=100, nrow=100)
dim(matC)


#(b)

meanNV<-apply(matNV, MARGIN=1, FUN=mean)
varNV<-apply(matNV, MARGIN=1, FUN=var)

meanC<-apply(matC, MARGIN=1, FUN=mean)
varC<-apply(matC, MARGIN=1, FUN=var)


#(c)

par(mfrow=c(2,2))
hist(meanNV, main="Mittelwert normalverteilter ZV")
hist(meanC, main="Mittelwert cauchy-verteilter ZV")
hist(varNV, main="Varianz normalverteilter ZV")
hist(varC, main="Varianz cauchy-verteilter ZV")
dev.off()
