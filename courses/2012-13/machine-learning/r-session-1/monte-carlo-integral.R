
# Annahme:  Dichte p(x) ist bekannt

# Ziel: Berechnung von E(f(X)) = integral f(x)*p(x)*dx


######## 

# Beispiel:  Gamma-Verteilung


x = rgamma(1000, shape=4.4, scale = 4)
hist(x, breaks=20)


#########

# E( x )  = ?

# theoretischer Wert:  
# shape*scale = 4.4*4 = 17.6

# numerische integration
f = function(x) x
h = function(x) f(x)*dgamma(x, shape=4.4, scale = 4)
integrate(h, lower=0, upper=Inf)


# monte carlo
B = 100000
x = rgamma(B, shape=4.4, scale = 4)

mean(f(x))

#######

# E( sqrt(x)*cos(x) ) = ?

# theoretischer Wert: nicht analytisch integrierbar!

# numerische integration
f = function(x) sqrt(x)*cos(x)
h = function(x) f(x)*dgamma(x, shape=4.4, scale = 4)
integrate(h, lower=0, upper=Inf)
# 0.003853785 with absolute error < 5.2e-05

# monte carlo
B = 1000000
x = rgamma(B, shape=4.4, scale = 4)

mean(f(x))

# MC Simulation mehrmals laufen lassen!
# Was folgt daraus ?

