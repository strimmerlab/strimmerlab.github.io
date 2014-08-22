#Loesung des fuenften Uebungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#11. November 2009
########################################################################

########################################################################
########################################################################



########################################################################
#Aufgabe 1
########################################################################


test <- treff(1000)
prop.table(table(test))

treff2 <- function(rep){
x <- rep(NA,rep)
for(i in 1:rep){	
x[i] <- rbinom(n=1, size = 1, prob = 0.70)
}
return(x)
}

#Simulation erfolgt über die Binomialverteilung mit der Wahrscheinlichkeit 0.7
#Mehr über Simulationen und Verteilungen am 25.11

########################################################################
#Aufgabe 2
########################################################################

geb <- function(n){
1-exp(lfactorial(365) - lfactorial(365-n) -n*log(365))
}

plot(1:100, geb(1:100), main = "Geburtstagsparadoxon", 
 xlab="Anzahl der Leute", ylab = "Wahrscheinlichkeit eines identischen  Geburtstages")


########################################################################
#Aufgabe 3
########################################################################

#Die Funktion zum Zaehlen:

za <- function (data, lower, upper){
count <- 0	
	for(j in 1:length(data)){
	    for(i in 1:length(data[,j])){
		 if(is.numeric(data[ ,j])){
	         if( ((data[i,j] > lower) &  (data[i,j] < upper)) & (!is.na(data[i,j])) ){
		count = count +1
		#print(count)		
		}		
		}
				
	    }
	}

count = count + 1
count
}



#Beispiel: Wieviele Werte des Datensatzes liegen im Intervall 20 bis 30

data<-read.csv(file="Daten/oecdM", header=TRUE, na.strings="NA")
#data<-read.csv(file="Daten/oecdM", header=TRUE, encoding = "UTF-8") 

attach(data)
head(data)
names(data)

za(data, 20, 30)


