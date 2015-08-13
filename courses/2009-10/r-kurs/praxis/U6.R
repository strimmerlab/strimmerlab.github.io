#Loesung des sechsten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#20. November 2009
########################################################################


########################################################################
#Aufgabe 1
########################################################################


#(a)
test <- rnorm(100, mean = 0, sd = 1)

#(b)

sim <- function(mu = 0, sigma = 1){
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
ew <- (sim(mu = 100, sigma = 2))$mu
plot(ew, pch = 17)

#(d) 
par(mfrow=c(5,2))
for(i in 1:10){ 
ew <- (sim(mu = 100, sigma = 2))$mu
plot(ew, pch = 17, ylim = c(99.0, 101.0))
lines(c(1,6),c(100,100), col=6, lty=3)
}


###########################
#Es ist zu sehen, dass bei geringer Fallzahl der beobachtete Erwartungswert
#starke Abweichungen vom wahren Erwartungswert hat.
#Je groesser die Fallzahl, umso genauer der beobachtete Erwartungswert.



########################################################################
#Aufgabe 2
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
