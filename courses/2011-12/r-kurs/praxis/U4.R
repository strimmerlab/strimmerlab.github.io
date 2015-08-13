#Loesung des vierten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#3. November 2011
########################################################################

########################################################################
########################################################################




########################################################################
#Aufgabe 1
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

par(mfrow=c(1,1))

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

########################################################################
#Aufgabe 3
########################################################################

#(a) 
# 1 Min = 6 Versuche
################
# 0 bis 6 "Erfolge"
v = 0:6
# Erfolgswahrscheinlichkeit = 0.75
p = dbinom(v,6,0.75)
# Zurückgelegte Wegstrecke (von -6 bis 6 in zweier-Schritten!)
x = 2*v - 6

##
#(b) 
plot(x,p)
which.max(p) #6
#Wahrscheinlich zurückgelegte Wegstrecke
x[which.max(p)] # 4
#=> Wahrscheinlich legt der Student 4 Meter zurück

# 2 Min = 12 Versuche
################
# 0 bis 12 "Erfolge"
v = 0:12
# Erfolgswahrscheinlichkeit = 0.75
p = dbinom(v,12,0.75)
# Zurückgelegte Wegstrecke (vo -12 bis 12 in zweier-Schritten!)
x = 2*v - 12
##

#(b) 
plot(x,p)
which.max(p) #10
#Wahrscheinlich zurückgelegte Wegstrecke
x[which.max(p)] # 6
#=> Wahrscheinlich legt der Student 6 Meter zurück

 
