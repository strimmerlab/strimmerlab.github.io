#Lösung des zehnten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#15. Dezember 2011
########################################################################



########################################################################
#1. Aufgabe: Lineare Regression mit Faktoren
########################################################################

 #(a) 
 data<-read.csv("Daten/Spielzeugautos")

 #(b) 
 data$car = as.factor(data$car)
 attach(data)
 
 #(c)
 boxplot(distance ~ car)

 #(d)
 lm.car <- lm(distance~angle+car)
 
 #(e) 
 summary(lm.car)

 #(f) 
 pdf(file="DiagnoseCARS.pdf", width=12, height=12)
 par(mfrow = c(2,2))
 plot(lm.car)
 dev.off()


########################################################################
#2. Aufgabe:  Interaktion von stetigen Variablen
########################################################################
 #(a) 
 suess <- read.table("Daten/Suess.csv", header = TRUE)
 attach(suess)

 #(b)  
 coplot(Geschmack ~ Feuchtigkeit | Suesse,  pch = c(5,18), rows = 1, columns = 3)

 #(c)

 lm.ohneI <- lm(Geschmack ~ Feuchtigkeit + Suesse)
 plot(Feuchtigkeit*Suesse, lm.ohneI$res)
 
 #Es ist ein Zusammenhang erkennbar 

 #(d)
 lm.mitI <- lm(Geschmack ~ Feuchtigkeit * Suesse)
 plot(Feuchtigkeit*Suesse, lm.mitI$res)
 #Es ist kein Zusammenhang erkennbar 
 summary(lm.mitI)
 # => Der Interaktionsterm ist signifikant


########################################################################
#3. Aufgabe: Diagnoseplots
########################################################################

 h1<-seq(1,10,0.05)
 n<-length(h1)
 X<-h1+rnorm(n, mean=0, sd=1)


#3a
######################

 epsilon1<-rnorm(n, mean=0, sd=1)
 Y1<-log(X)+epsilon1
 lm1<-lm(Y1~X)


 pdf(file="DiagnoseLOG.pdf", width=12, height=12)
 par(mfrow = c(2,2))
 plot.lm(lm1)
 dev.off()

#3b
######################

 epsilon2<-rcauchy(n, location=0, scale=1)
 plot(density(epsilon2), col="red")
 lines(density(epsilon1), col="blue")

 Y2<-X+epsilon2
 lm2<-lm(Y2~X)

 pdf(file="DiagnoseCAUCHY.pdf", width=12, height=12)
 par(mfrow = c(2,2))
 plot.lm(lm2)
 dev.off()


#3c
######################

 epsilon3<-rnorm(n, mean=rep(0,n), sd=(X/10))
 Y3<-X+ epsilon3


 lm3<-lm(Y3~X)
 plot.lm(lm3, which=3)


 pdf(file="DiagnoseERR.pdf", width=12, height=12)
 par(mfrow = c(2,2))
 plot.lm(lm3)
 dev.off()

















