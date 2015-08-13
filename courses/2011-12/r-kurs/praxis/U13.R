#Loesung des 13. Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#19. Januar 2012
########################################################################


#Aufgabe 1
########################################################################
########################################################################

#(a)

data.all<-load("Daten/golub.RData")
dim(golub)
#3051   38
head(golub)

golub.cl
golub.gnames[1:10,]
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL","AML"))



id.training<- unique(round(runif(50, min=1, max=38)))[1:22]
id.training<-sort(id.training)


g.tr<-golub[,id.training]
dim(g.tr)
#[1] 3051   22

fac.tr<-gol.fac[id.training]


g.test<-golub[,-id.training]
fac.test<-gol.fac[-id.training]



#(b)
#Fold Change für zwei getrennte Matrizen AML und ALL


golub.AML<-g.tr[, which(fac.tr=="AML")]
golub.ALL<-g.tr[, which(fac.tr=="ALL")]

mu.ALL<-apply(golub.ALL, MARGIN=1, mean)
mu.AML<-apply(golub.AML, MARGIN=1, mean)

fold.chg<-mu.ALL-mu.AML





#(c)
#t-score als Fold Change/Standardabweichung

var.gen<-apply(golub, MARGIN=1, var)
t.score<-fold.chg/(sqrt(var.gen))




#(d)
#Rangliste
#Mit Nummern

ranking.t<-order(abs(t.score), decreasing=TRUE)


ranking.t[1:20]
ranking.fc<-order(abs(fold.chg), decreasing=TRUE)
ranking.fc[1:20]


g.tscore <- t(g.tr[ranking.t[1:20],])
dim(g.tscore)
# 22 20

g.test.tscore <- t(g.test[ranking.t[1:20],])
dim(g.test.tscore)
# 20 16


#(e)
#Erstellen der Vorhersageregel


library(MASS)
golub.lda <- lda(g.tscore, fac.tr)

## prediction
fac.predict = predict(golub.lda, newdata=g.test.tscore)$class

#(f)
#Bewertung der Vorhersageregel

## prediction error
pred.err = sum(fac.predict != fac.test)/length(fac.test)
pred.err
#0.125

## Wie viele ALL Stichproben sind falsch klassifiziert? (
pred.err.ALL = 1-length(intersect(which(fac.predict == "ALL"), which(fac.test== "ALL")))/length(fac.test== "ALL")
pred.err.ALL
#0.3125, 0.4375 abhängig von Aufteilung in Test/Trainingsdaten

## Wie viele AML  Stichproben sind falsch  klassifiziert?
pred.err.AML = 1- length(intersect(which(fac.predict == "AML"), which(fac.test== "AML")))/length(fac.test== "AML")
pred.err.AML
#  0.8125,  0.6875 abhängig von Aufteilung in Test/Trainingsdaten


