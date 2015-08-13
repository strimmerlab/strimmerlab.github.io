# geoR examples 

library("geoR")

########

# Surface elevation data
data(elevation)

summary(elevation)
points(elevation)
plot.geodata(elevation)
plot.geodata(elevation, lowess=TRUE)


# variogram cloud
vario.c = variog(elevation, max.dist=1, op="cloud")
# binned variogram
vario.b = variog(elevation, max.dist=1)
# smoothed variogram
vario.s = variog(elevation, max.dist=1, op="sm", band=0.2)


# plotting the variograms:
par(mfrow=c(1,3))
plot(vario.c, main="variogram cloud")
plot(vario.b, main="binned variogram") 
plot(vario.s, main="smoothed variogram") 
par(mfrow=c(1,1))


########

# Swiss rainfall data 
library("geoR")
data(SIC)


points(sic.100)
points(sic.all, borders=sic.borders)

########

data(s100)

# parametric fit of Matern covariance function

vario100 = variog(s100, max.dist=1)
ini.vals = expand.grid(seq(0,1,l=5), seq(0,1,l=5))
ols = variofit(vario100, ini=ini.vals, fix.nug=TRUE, wei="equal")
summary(ols)
wls = variofit(vario100, ini=ini.vals, fix.nug=TRUE)
summary(wls)
plot(vario100)
lines(wls)
lines(ols, lty=2)

# spatial prediction using krigin

par(mfrow=c(1,2))
points(s100)
pred.grid = expand.grid(seq(0, 1, l = 51), seq(0, 1, l = 51))
ml = likfit(s100, ini = c(1, 0.5), fix.nugget = TRUE)
kc = krige.conv(s100, loc = pred.grid, krige = krige.control(obj.m = ml))
image(kc, loc = pred.grid, col = gray(seq(1, 0.1,  l = 30)), 
 xlab = "Coord X", ylab = "Coord Y")
par(mfrow=c(1,1))


