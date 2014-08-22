#Loesung des zwoelften Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#13. Januar 2011
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


