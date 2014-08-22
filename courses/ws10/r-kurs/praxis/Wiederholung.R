#########################################################################
#########################################################################
#TESTEN
#########################################################################
#########################################################################



########################################################################
#Aufgabe 1 von U 7
########################################################################
########################################################################

data<-read.csv(file="PISA.csv", header=TRUE)

attach(data)

pdf(file="PISAhistogramm.pdf", width=13, height=6)

boxplot(R00, R06,   M00, M06,  S00, S06, names=c("Reading00", "Reading06",   "Mathematics00", "Mathematics06",  "Science00", "Science06"))

dev.off()

#Es ist zu erkennen, dass der PISA-Score zur Lese und Kompetenz in der Mathematik
#abgenommen hat; Im Gegensatz dazu ist der mittlere PISA-Score in den Naturwissenschaften gestiegen
#Daraus ergeben sich die t-Test (!Messwiederholung! mit "paired=TRUE") mit folgenden Alternativhypothesen

t.test(R00, R06, paired=TRUE, alternative="greater")
t.test(M00, M06, paired=TRUE, alternative="greater")
t.test(S00, S06, paired=TRUE, alternative="less")

#Die Lesekompetenz hat sich signifikant verschlechtert!


#Vorsicht beim gepaarten t-Test wird die Mittelwertsdifferenz betrachtet
#Beispiel: alternative="greater" hat folgende Hypothesen
#H0: Die Mittelwertsdifferenz ist kleiner oder gleich Null (dh Score in 2000 <= Score 2006)
#H1: Die Mittelwertsdifferenz ist groesser Null (dh Score in 2000 > Score 2006)






#Aufgabe 2 von U 7
########################################################################
########################################################################


X1<-rexp(100, rate=0.1)
X2<-20-rexp(100,rate=0.1)

#Die Mittel bzw Erwartungswert der beiden Objekte sind gleich! 
#Denn E(X1)=10=1/0.1 = 20-E(X2)=20-10

pdf(file="EXPtesten.pdf", width=13, height=6)
plot(density(X1), ylim=c(0,0.15))
lines(density(X2),col="red")

dev.off()

mean(X1)
mean(X2)

t1<-t.test(X1,X2)
t1_np<-wilcox.test(X1,X2)


#Mit dem Kerndichteschaetzer sind deutliche Unterschiede zwischen den Objekten X1 und X2 zu sehen
#Mit einem t-Test koennen mit sehr hoher Wahrscheinlichkeit diese Unterschiede nicht gezeigt werden,
#da X1 und X2 nicht normalverteilt sind
#Aber mit dem nonparametrischen Wilcoxen Test sind diese Unterschiede zu finden!

#Einschub: Mit dem Kolmogorov-Smirnov Test kann untersucht werden,
#ob eine Zufallsvariable einer bestimmten Verteilung entspricht.
#Test auf Normalverteilung:
ks.test(X1, y="pnorm", mean=mean(X1), sd=sd(X1))
ks.test(X1, y="pexp", rate=0.1)
ks.test(X2, y="pnorm", mean=mean(X2), sd=sd(X2))
#Da Zufallszahlen gezogen werden, kann es sein dass in Ausnahmefaellen abweichende Testergebnisse auftreten!



#Aufgabe 1 von U 8
########################################################################
########################################################################

data<-read.csv(file="google.csv", header=TRUE)

#a) Umwandeln von OW als Faktor

data[,2]<-factor(data[,2], levels=c(0,1), labels=c("Ost", "West"))
data[,2]

attach(data)


#b) Test ob Krise 2009 häufiger oder gleich häufig gesucht wird wie 2008 

boxplot(Krise08, Krise09)
t.test(Krise08, Krise09, paired=TRUE, alternative="greater")
wilcox.test(Krise08, Krise09, paired=TRUE, alternative="greater",  exact=FALSE)


#Alternativhypothese: Mittelwert(Krise08)>Mittelwert(Krise09)
#enspricht 	      Mittelwert(Krise08-Krise09)>0

#Interpretation: Die Nullhypothese kann zu einem Signifikanzniveau von 5% abgelehnt werden.
#dh. mit einer Fehlerwahrscheinlichkeit von 5% wurde 2009 nicht staerker oder gleich stark
#nach dem Begriff Krise gegoogelt.


#c) Test auf Ost-West Unterschiede
# neuer Indikator für den Unterschied, zwei Möglichkeiten: Differenz und Quotient

difK<-(Krise08-Krise09)
quotK<-(Krise08/Krise09)

boxplot(difK~OW)
t.test(difK~OW, paired=FALSE, alternative="greater")
wilcox.test(difK~OW, paired=FALSE, alternative="greater",  exact=FALSE)



boxplot(quotK~OW)
t.test(quotK~OW, paired=FALSE, alternative="greater")
wilcox.test(quotK~OW, paired=FALSE, alternative="greater",  exact=FALSE)


#Alternativhypothese: Mittelwert(West)<Mittelwert(Ost)
#enspricht 	      Mittelwert(West-Ost)<0

#Interpretation: Die Nullhypothese kann zu einem Signifikanzniveau von 5% NICHT abgelehnt werden.



#d) Test ob Arbeitsamt im Westen  häufiger oder gleich häufig gesucht wird wie im Osten


boxplot(Arbeitsamt~OW)
t.test(Arbeitsamt~OW, paired=FALSE, alternative="less")
wilcox.test(Arbeitsamt~OW, paired=FALSE, alternative="less",  exact=FALSE)


#Alternativhypothese: Mittelwert(West)<Mittelwert(Ost)
#enspricht 	      Mittelwert(West-Ost)<0

#Interpretation: Die Nullhypothese kann zu einem Signifikanzniveau von 5% abgelehnt werden.
#dh. mit einer Fehlerwahrscheinlichkeit von 5% wurde 2010 im Westen nicht staerker oder gleich stark
#nach dem Begriff Arbeitsamt gegoogelt als im Osten.


detach(data)


#########################################################################
#########################################################################
#Regression
#########################################################################
#########################################################################

########################################################################
#1. Aufgabe von U 10: Lineare Regression mit Faktoren
########################################################################

 #(a) 
 library(DAAG)
 data<-toycars


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
#3. Aufgabe von U 10: Diagnoseplots
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

#########################################################################
#########################################################################
#ANOVA
#########################################################################
#########################################################################


########################################################################
#1. Aufgabe: Einfaktorielle ANOVA
########################################################################

 #(a) 
 hot <- read.table("HotDog.csv", header = TRUE)
 attach(hot)
 
 #(b)
 anova.cal <- lm(Calories ~ Type)
summary(anova.cal)
  anova(anova.cal)

 #(c)
 anova.sod <- lm(Sodium ~ Type)
 summary(anova.sod)
 anova(anova.sod)
 
 #(c)

 pdf(file="BP_meatismurder.pdf", width=12, height=6)
 par(mfrow = c(1,2))
 boxplot(Calories~Type, main="Calories")
 boxplot(Sodium~Type, main="Sodium")
 dev.off()



########################################################################
#2. Aufgabe:  Mehrfaktorielle ANOVA
########################################################################
 #(a) 
  noise <- read.table("Noise.csv", header = TRUE)
  attach(noise)
 #(b)
  an.noise1 <- lm(NOISE ~ SIZE	* TYPE * SIDE)
  summary(an.noise1)
  anova(an.noise1)
 
  #(c)

  an.noiseI <- lm(NOISE ~ SIZE	* TYPE)
  anova(an.noiseI)
  summary(an.noiseI)

  an.noiseOhneI <- lm(NOISE ~ SIZE + TYPE)
  summary(an.noiseOhneI)
  anova(an.noiseOhneI)
   
 # => Interaktion nicht signifikant

  #(d)
  #Nein, eher nicht, "Size" ist der dominierende Faktor, "Type" ist zwar nicht signifikant
  # verbessert aber das R^2 trotzdem 

    an.red <- lm(NOISE ~ SIZE)
  summary(an.red)
  anova(an.red)
 
  # "Type" hat zwar einen geringeren Einfluss als "Size" ist aber nicht unwichtig 



