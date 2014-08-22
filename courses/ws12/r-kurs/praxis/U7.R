#Loesung des siebten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#22. November 2012
########################################################################

#Aufgabe 1
########################################################################
#######################################################################

data<-read.csv(file="/home/bklaus/uni/Teaching/R-Kurs/R-Kurs 2011/Daten/PISA.csv", header=TRUE)

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






#Aufgabe 2
########################################################################
########################################################################

data<-read.csv(file="Daten/google.csv", header=TRUE)

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

#Aufgabe 3
########################################################################
########################################################################

X1<-rnorm(100, mean=0, sd=1)
X2<-rnorm(100, mean=0, sd=1)

t1<-t.test(X1,X2)
t1$statistic



repeat.t<-function(n, mu1=0, sigma1=1, mu2=0, sigma2=1){
res<-rep(0,n)
for(i in 1:n){
X1<-rnorm(100, mean=mu1, sd=sigma1)
X2<-rnorm(100, mean=mu2, sd=sigma2)
t1<-t.test(X1,X2)
res[i]<-t1$statistic
}
return(res)

}


tstat.vektor<-repeat.t(1000)
plot(density(tstat.vektor))
hist(tstat.vektor)
sum(abs(tstat.vektor)>1.972)

sum(abs(tstat.vektor)>1.972)/1000

#Der Prozentsatz der Falsch Positiven Werte (alpha Fehler) sollte
#stets im Bereich um 0.05 sein

#Zur Berechnung des kritischen (zweiseitigen) Bereiches wurde R verwendet: 
#qt(0.975,df=198)
#1.972017




#Aufgabe 4
########################################################################
########################################################################
#Daten einlesen
dat <- read.csv("Hustensaft.csv")
attach(dat)
#t-Test zum Vergleich mir Normwert
t.test(Kon, mu = 40, alternative = "less") 

# H_0 kann (knapp) nicht abgelehnt werden, p-Wert > 0.05
# => Probe im Normbereich!
