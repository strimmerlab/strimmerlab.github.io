

# Monte Carlo Berechnung von Pi

B = 1000  # Zahl der Wiederholungen

# simulierte Würfe auf Dartbrett 
x = runif(B, min=-1, max=1)
y = runif(B, min=-1, max=1)

# welche sind innerhalb des Kreises
innen = ( x^2+y^2 ) <= 1
innen

# relativer Anteil
mean(innen) 

pi/4



###########################################

# alles in einer Funktion

simPI = function(B=1000)
{
  x = runif(B, min=-1, max=1)
  y = runif(B, min=-1, max=1)
  return ( 4* mean( ( x^2+y^2 ) <= 1 ) )
}

simPI()

############################################

# Fehlerabschätzung (W = 1000)


# Bias and Variance

W = 1000
pi.vec = rep(NA, W)
for (i in 1:W)
  pi.vec[i] = simPI()

hist(pi.vec, breaks=20)

# BIAS
mean(pi.vec) - pi

# SD
sd(pi.vec)



############################################

# Fehlerabschätzung (W = 10000)


# Bias and Variance

W = 10000
pi.vec = rep(NA, W)
for (i in 1:W)
  pi.vec[i] = simPI()

hist(pi.vec, breaks=20)

# BIAS
mean(pi.vec) - pi

# SD
sd(pi.vec)


# Vergleich der beiden SDs - was bedeutet das ???







