
# Scaled Inverse Chi-Square Distribution

f <- function(x,n=5,a=4) x^(-n/2)*exp(-a/(2*x))

###########

# Metropolis Sampler
met <- numeric(5000)
last <- 1
for (i in 1:5000) 
{
	cand<-runif(1,0,100) 
        alpha <- f(cand,5,4)/f(last,5,4) 

        if (runif(1) < min(alpha, 1)) 
          last <- cand
        met[i]<- last
}

par(mfrow=c(1,3))
hist(met, bre=50, freq=FALSE, xlim=c(0,20))
plot(f, 0, 20)
plot(met)
par(mfrow=c(1,1))

###########

# Metropolis-Hastings Sampler
met <- numeric(5000)
last <- 1
for (i in 1:5000) 
{
	cand<-rchisq(1,5) # Vorschlagsdichte (nicht symmetrisch)
	alpha <- (f(cand,5,4)/f(last,5,4))*( dchisq(cand,5)/dchisq(last,5))
 
        if (runif(1) < min(alpha, 1)) 
          last <- cand
        met[i]<- last
}

	
par(mfrow=c(1,3))
hist(met, bre=50, freq=FALSE, xlim=c(0,20))
plot(f, 0, 20)
plot(met)
par(mfrow=c(1,1))

#########

# vergleiche die beiden Verfahren. Welcher ist besser ?




