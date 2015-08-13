# Skript "prob.R"
# http://www.stat.wisc.edu/~larget/R/prob.R
########################################################################

source("prob.R")
setwd("Desktop")

#######################
# Binomialverteilung
########################


# Dichte der Binomialverteilung
dbinom(0:5, 5, 0.1)

# Für einen  Fußballspieler, der 10 mal aufs Tor schießt
# und mit 25% Wahrscheinlichkeit trifft, wie warscheinlich sind
# 5 Treffer? 
dbinom(5, 10, 0.25)
# ... mehr als  3 Treffer
1- pbinom(2, 10, 0.25)
pbinom(2, 10, 0.25, lower.tail = FALSE)

gbinom(10, 0.25, a = 3, b = 10, scale = T)
#dbinom(1, 4, 1/6)
# zwischen 45 und 55 mal Kopf beim Münzwurf:
gbinom(100, 0.5, a = 45, b = 55, scale = T)
# 75% Quantil (3.Quartil)
qbinom(0.75, 200, 0.3)
gbinom(200, 0.3, scale = T, quantile = 0.75)
#Stichprobe
rbinom(20, 10, 0.5)


### Warscheinlichkeit 201 oder mehr von 600 multiple Choice
###Fragen richtig zu raten
plot(1-pbinom(1:200, 600, 0.25))
### Warscheinlichkeit 101 oder mehr von 500 multiple Choice
###Fragen richtig zu raten
plot(1-pbinom(1:100, 500, 0.25))
###############################
#Gleichverteilung (stetig)
###############################
#ziehen
set.seed(32078)
runif(20,0,1)

#Mittelwert und Varianz
 r <- runif(10, 1, 2)
  mean(r) #theo 1.5
   var(r) #theo 1/12 = 0.833
#Wahrscheinlichkeit, dass ZV größer als 1.5 (theo = 0.5)
length(r[r>1.5])/length(r)


###############################
#Poisson
###############################
# Grenze der Binomialverteilung
# für große n: B(n,p ) = Pois(np)

# 75% Quantil (3.Quartil)
qpois(0.75, 200*0.3)
gpois(200*0.3,  quantile = 0.75)

#Aproximation der Binomialverteilung

#n = 500, p = 0.5
hist(rbinom(10000, 500, 0.5), freq = F)
lines(dbinom(1:500, 500, 0.5), col = "RED")
lines(dpois(1:500, 500*0.5), col = "GREEN")

#Für unseren Fußballspieler
# lambda
lam = 10 * 0.25
dpois(5,2.5)
#vs
dbinom(5, 10, 0.25)

#Angenommen 12 Autos pro Minute fahren im Schnitt über eine
#Brücke, wie groß ist die Warscheinlichkeit, dass in einer Minute 
#mehr als 16 Autos über die Brücke fahren?

1-ppois(16, lambda=12)

###############################
#Normalverteilung
###############################
#Bsp: Planzenhöhe ~ N(145, 22)

#Anteil der Planzen, die größer als 100cm sind

1 - pnorm(100, 145, 22)
gnorm(145, 22, a = 100)



#zwischen  120cm und 150cm
pnorm(150, 145, 22) - pnorm(120, 145, 22)
gnorm(145, 22, a = 120, b = 150)

## kleinere Standardabweichung
gnorm(145, 5, a = 120, b = 150)

#150 oder weniger
pnorm(150, 145, 22)
gnorm(145, 22, b = 150)

#1. Quartil
qnorm(0.25, 145, 22)
gnorm(145, 22, quantile = 0.25)

#IQR
qnorm(0.75, 145, 22) - qnorm(0.25, 145, 22)
#graphisch
  iqr = qnorm(c(0.25, 0.75), 145, 22)
 gnorm(145, 22, a = round(iqr[1], 1), b = round(iqr[2], 1))





