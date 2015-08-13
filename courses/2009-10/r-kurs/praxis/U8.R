#Lösung des achten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#16. Dezember 2009
########################################################################

########################################################################
########################################################################
########################################################################
########################################################################
#Aufgabe 1
########################################################################
########################################################################

data<-read.csv(file="Daten/PISA", header=TRUE)

attach(data)


boxplot(R00, R06,   M00, M06,  S00, S06, names=c("Reading00", "Reading06",   "Mathematics00", "Mathematics06",  "Science00", "Science06"))

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


X1<-rexp(100, rate=0.1)
X2<-20-rexp(100,rate=0.1)

#Die Mittel bzw Erwartungswert der beiden Objekte sind gleich! 
#Denn E(X1)=10=1/0.1 = 20-E(X2)=20-10

plot(density(X1), ylim=c(0,0.15))
lines(density(X2),col="red")

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

data<-read.csv(file="Daten/Weihnachtsmarkt", header=TRUE)
attach(data)

#H0: Es ist mehr als oder genau gleich 0.25l Gluehwein in den Glaesern
#H1: Es ist zu wenig Gluehwein in den Glaesern

t.test(Inhalt,  alternative="less", mu=0.25)

detach(data)


#Der p-Wert liegt bei 0.05667
#Somit kann die Nullhypothese, dass mindestens 0.25l Gluehwein
#in den Glaesern ist nicht abgelehnt werden. Daraus koennen die
#Kontrolleure nicht folgern, dass zu wenig ausgeschenkt wurde




