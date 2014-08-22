
# lade R Standardbibliothek für Bootstrap Analysen
library("boot")

################################################
# erstes Beispiel: Normalverteilung
################################################

# true model
N = 100
x = rnorm(N, mean=2.34, sd=1.2)
hist(x)

#  Mittelwert
m = mean(x)  


#### Was is der Bias und die Varianz von m?

# Theorie: Bias = 0, Varianz = 1.2*1.2/100 = 0.0144 

#### Bootstrap Schätzung

statistic = function(d, w) mean(d[w])
x.boot <- boot(x, statistic, R = 999)
plot(x.boot)
print(x.boot)

# Bias 
mean(x.boot$t) - x.boot$t0

# Variance
var(x.boot$t)

# Konfidenzintervalle
# "norm" = theoretisches Interval unter Normalverteilungsannahme
boot.ci(x.boot, conf = c(0.90,0.95),
        type = c("norm","basic","perc","bca"))


################################################
# zweites Beispiel: Bevölkerungsgröße
################################################

data(city)

plot(city, xlab="1920 population", ylab="1930 population")

# Frage: Konfidenzintervall für Bevölkerungswachstum?

ratio <- function(d, w) sum(d$x * w)/sum(d$u * w)
city.boot <- boot(city, ratio, R = 999, stype = "w",sim = "ordinary")
boot.ci(city.boot, conf = c(0.90,0.95),
        type = c("norm","basic","perc","bca"))


