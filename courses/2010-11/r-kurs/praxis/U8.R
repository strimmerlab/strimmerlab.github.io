#Loesung des achten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#2. Dezember 2010
########################################################################

########################################################################
########################################################################
########################################################################
########################################################################
#Aufgabe 1
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

#Aufgabe 2
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



#Aufgabe 3
########################################################################
########################################################################
#(a)

blut <- read.table("Calcium.csv" )
attach(blut)

#(b)

qqnorm(blut$Decrease ~ Treatment )

#(c)
qqnorm(Decrease[Treatment == "Placebo"] )
qqline(Decrease[Treatment == "Placebo"], col = "green" )

qqnorm(Decrease[Treatment == "Calcium"] )
qqline(Decrease[Treatment == "Calcium"], col = "green" )

#=> keine Abweichung von der Normalverteilung in den Gruppen erkennbar

sd(Decrease[Treatment == "Placebo"])
sd(Decrease[Treatment == "Calcium"])

var.test(sd(Decrease[Treatment == "Placebo"]), sd(Decrease[Treatment == "Calcium"]))

#=> Varianzunterschiede nicht allzu groß! => Gleichheit



t.test(Decrease[Treatment == "Placebo"], Decrease[Treatment == "Calcium"], alternative = "l",
paired = FALSE, var.equal = TRUE) 

#=> Einseitiger t-Test: H_a : "Die Blutdrucksenkung in der Placebo-Gruppe ist
#kleiner als die in der Calcium Gruppe" 


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




