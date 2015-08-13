#Loesung des zwoelften Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#12. Januar 2012
########################################################################


#Aufgabe 1
########################################################################
########################################################################

#(a)

data.all<-load("Daten/golub.RData")
dim(golub)
head(golub)

golub.cl
golub.gnames[1:10,]
gol.fac <- factor(golub.cl, levels=0:1, labels = c("ALL","AML"))


#(b)
#Spannweite über den apply Befehl

range.gen<-apply(golub, MARGIN=1, max)-apply(golub, MARGIN=1, min)
hist(range.gen)

#(c)
#Varianz über den apply Befehl

var.gen<-apply(golub, MARGIN=1, var)
golub.gnames[order(var.gen, decreasing=TRUE)[1:10], ]
par(mfrow=c(2,5))
for(i in 1:10){
boxplot(golub[order(var.gen, decreasing=TRUE)[i],])
}


par(mfrow=c(1,1))

#Aufgabe 2
########################################################################
########################################################################

#Fold Change für zwei getrennte Matrizen AML und ALL

#(a)

golub.AML<-golub[, which(gol.fac=="AML")]
golub.ALL<-golub[, which(gol.fac=="ALL")]

mu.ALL<-apply(golub.ALL, MARGIN=1, mean)
mu.AML<-apply(golub.AML, MARGIN=1, mean)

fold.chg<-mu.ALL-mu.AML

#(b)

length(which(fold.chg<0))
list.negex<-golub.gnames[which(fold.chg<0),]
list.negex[1:10,]



#(c)

#t-score als Fold Change/Standardabweichung

t.score<-fold.chg/(sqrt(var.gen))




#(d)
#Rangliste
#Mit Nummern

ranking.t<-order(t.score, decreasing=TRUE)
ranking.t[1:20]
ranking.fc<-order(fold.chg, decreasing=TRUE)
ranking.fc[1:20]


#Mit Namen

ranking.names.t<-golub.gnames[order(t.score, decreasing=TRUE),2]
ranking.names.t[1:20]
ranking.names.fc<-golub.gnames[order(fold.chg, decreasing=TRUE),2]
ranking.names.fc[1:20]

# Schnittmenge

match(ranking.names.t[1:20], ranking.names.fc[1:20], nomatch = 0)


#(e)
# Histogramm vom t-score

hist(t.score, breaks=75)


#(f)
#90 % der Gene sind ...
ninety = floor(0.90*length(golub.gnames[,1 ]))
 
#  90% der gene nach t-score georndet
t.ninety = t.score[order(abs(t.score), decreasing=FALSE)[1:ninety]]

mean.t.ninety = mean(t.ninety)

sd.t.ninety = sd (t.ninety)

mean.t.ninety
sd.t.ninety

hist(t.score, breaks=75, freq = FALSE, ylim = c(0,0.8))
lines(sort(t.ninety), dnorm(sort(t.ninety), mean = mean.t.ninety, sd = sd.t.ninety), lwd = 2, col = "darkgreen")
