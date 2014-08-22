#Lösung des achten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#29 November 2012
########################################################################


####Aufgabe 1 
########################################################################
 #(a) 

 data<-read.csv("Daten/Spielzeugautos")


 data$car = as.factor(data$car)
 attach(data)


 #(b) 
 boxplot(distance ~ car)

 #(c)
 lm.car1 <- lm(distance~angle, subset = which(car==1))
 summary(lm.car1)

 lm.car2 <- lm(distance~angle, subset = which(car==2))
 summary(lm.car2)

 lm.car3 <- lm(distance~angle, subset = which(car==3))
 summary(lm.car3)

 #(d) 
 par(mfcol=c(1,3))

 plot(angle[which(car==1)], distance[which(car==1)] )
 abline(lm.car1$coefficients, col = "red")

 plot(angle[which(car==2)], distance[which(car==2)] )
 abline(lm.car2$coefficients, col = "red")

 plot(angle[which(car==3)], distance[which(car==3)] )
 abline(lm.car3$coefficients, col = "red")

 par(mfcol=c(1,1))
 
 #(e) 
 plot(lm.car1)
 plot(lm.car2)
 plot(lm.car3)

####Aufgabe 2
########################################################################

# (a)
HerData <- read.csv("Herz.csv")
plot(HerData)
attach(HerData)

# (b)
# lineare Regression
lm.Herz <- lm(Herzfrequenz  ~ Alter)
summary(lm.Herz)
plot(HerData)
abline(lm.Herz$coefficients, col = "red")

# Modell: Herzfrequenz = 210 - 0.8*Alter


