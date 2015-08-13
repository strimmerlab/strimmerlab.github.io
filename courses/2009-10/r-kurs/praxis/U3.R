#Loesung des dritten Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#5. November 2009
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

#g
########################################################################

M <- mean(Bullying, na.rm = TRUE)

SD <- sd(Bullying, na.rm = TRUE)

row.names(data)[which( (Bullying > M + SD) )]

#h
########################################################################

ind<- order(Einkommen)	  
#Index der die Ränge der Länder bezüglich des Einkommen enthält
data2<-data[ind,]
#neuer nach ind angeordneter Datensatz


write.csv(data2, file="Daten/data2")
#Hinweis: R fügt in diesen Datensatz ein zusaetzliches "" ein, wenn
#man ihn im urspruenglichen Format herausschreiben will, muss man 
#den Befehl
write.table(data2, file="data2", sep=",")
nutzen


#i
########################################################################
freq <- table(data$Geo)
pie(freq, col = c("green", "blue"))

#j
########################################################################
stripchart(data$Lesen~data$Geo, vertical = TRUE)


########################################################################
########################################################################
#Aufgabe 2
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

#Für Werte, die groesser als das 75% Quantil sind, steigt die Kurve offensichtlich stark an.

#e
########################################################################
sep  <- qua[3]

row.names(data)[which(data$Bildung > sep)]

########################################################################
########################################################################
#Aufgabe 3
########################################################################
########################################################################

#a
########################################################################

mad <- sapply(data[,1:15], mad, na.rm = TRUE)
sd <- sapply(data[,1:15], sd, na.rm = TRUE)


#b
########################################################################

diff <- sort(abs(mad-sd), decreasing = TRUE)


diff[1:2]

#d
########################################################################

#Es sind jeweils grosse Ausreisser vorhanden

