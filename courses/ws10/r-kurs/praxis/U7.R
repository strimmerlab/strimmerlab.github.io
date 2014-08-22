#Loesung des achten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#25.11. 2010
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


