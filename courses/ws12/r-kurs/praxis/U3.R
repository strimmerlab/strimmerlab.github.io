#Lösung des dritten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#25 Oktober 2012
########################################################################

########################################################################
########################################################################
########################################################################
########################################################################
#Aufgabe 1
########################################################################
########################################################################

#a
########################################################################

data<-read.csv(file="Daten/oecdM", header=TRUE, na.strings="NA")
#data<-read.csv(file="http://www.uni-leipzig.de/~zuber/teaching/ws11/r-kurs/praxis/oecd.csv", header=TRUE, na.strings="NA")

#data<-read.csv(file="Daten/oecdM", header=TRUE, encoding = "UTF-8") 
#Bei Windows muss die Kodierung "UTF-8" erzwungen werden, um die Umlaute richtig 
#einzulesen 

attach(data)
head(data)
names(data)

#b
########################################################################

sapply(data, mean, na.rm=TRUE)
sapply(data, var, na.rm=TRUE)

# bei listen immer saaply verwenden
# apply ist bei matrizen mit einheitlichem objekten zu verwenden


#c
########################################################################

 "Niederlande" %in% row.names(data)
 "China" %in% row.names(data)

#d
########################################################################

Alkohol[which(Alkohol==max(Alkohol, na.rm=TRUE))]
row.names(data)[which(Alkohol==max(Alkohol, na.rm=TRUE))]

#bzw.

row.names(data)[which.max(Alkohol)]

#e
########################################################################

Säuglsterblichkeit[which(Säuglsterblichkeit==min(Säuglsterblichkeit, na.rm=TRUE))]
row.names(data)[which(Säuglsterblichkeit==min(Säuglsterblichkeit, na.rm=TRUE))]

#f
########################################################################

Bewegung[which(Bewegung<mean(Bewegung, na.rm=TRUE))]
row.names(data)[which(Bewegung<mean(Bewegung, na.rm=TRUE))]



########################################################################
########################################################################
#Aufgabe 2
########################################################################
########################################################################


#a
########################################################################


freq <- table(data$Geo)
pie(freq, col = c("green", "blue"))

#b
########################################################################

stripchart(data$Lesen~data$Geo, vertical = TRUE)


########################################################################
########################################################################
#Aufgabe 3
########################################################################
########################################################################

#a
########################################################################

boxplot(data$Bildung)


#b
########################################################################

qua <- quantile(data$Bildung, prob = c(0.25,0.5,0.75,0.85,0.95,1.00))


#c
########################################################################

plot(sort(data$Bildung), type = "l")


#d
########################################################################

#Für Werte, die größer als das 75% Quantil sind, steigt die Kurve offensichtlich stark an.

#e
########################################################################

sep  <- qua[3]

row.names(data)[which(data$Bildung > sep)]




########################################################################
########################################################################
#Aufgabe 4
########################################################################
########################################################################

#a
########################################################################

search<-read.csv(file="Daten/google2011.csv", header=TRUE, na.strings="NA")
dim(search)
head(search)




plot(search[,1], type="l", col="orange", xlab="[Woche] seit 1. Januar 2011", ylab="Google-Abfrage", ylim=c(0,100))


#b
########################################################################

lines(search[,2],  col="blue")



#c
########################################################################

legend("topright", legend=c("zu Guttenberg", "Knut"), col=c("orange", "blue"), lty=1)

