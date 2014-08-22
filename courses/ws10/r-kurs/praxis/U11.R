#Lösung des elften Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#2. Februar 2011
########################################################################



########################################################################
#1. Aufgabe: Einfaktorielle ANOVA
########################################################################

 #(a) 
 hot <- read.table("HotDog.csv", header = TRUE)
 attach(hot)
 
 #(b)
 anova.cal <- lm(Calories ~ Type)
summary(anova.cal)
  anova(anova.cal)

 #(c)
 anova.sod <- lm(Sodium ~ Type)
 summary(anova.sod)
 anova(anova.sod)
 
 #(c)

 pdf(file="BP_meatismurder.pdf", width=12, height=6)
 par(mfrow = c(1,2))
 boxplot(Calories~Type, main="Calories")
 boxplot(Sodium~Type, main="Sodium")
 dev.off()



########################################################################
#2. Aufgabe:  Mehrfaktorielle ANOVA
########################################################################
 #(a) 
  noise <- read.table("Noise.csv", header = TRUE)
  attach(noise)
 #(b)
  an.noise1 <- lm(NOISE ~ SIZE	* TYPE * SIDE)
  summary(an.noise1)
  anova(an.noise1)
 
  #(c)

  an.noiseI <- lm(NOISE ~ SIZE	* TYPE)
  anova(an.noiseI)
  summary(an.noiseI)

  an.noiseOhneI <- lm(NOISE ~ SIZE + TYPE)
  summary(an.noiseOhneI)
  anova(an.noiseOhneI)
   
 # => Interaktion nicht signifikant

  #(d)
  #Nein, eher nicht, "Size" ist der dominierende Faktor, "Type" ist zwar nicht signifikant
  # verbessert aber das R^2 trotzdem 

    an.red <- lm(NOISE ~ SIZE)
  summary(an.red)
  anova(an.red)
 
  # "Type" hat zwar einen geringeren Einfluss als "Size" ist aber nicht unwichtig 


########################################################################
#3. Aufgabe: Down-Syndrom
########################################################################

#(a)

mice <- read.table("Mice.csv", header = TRUE)

mice$tg <- as.factor(mice$tg)
mice$sex <- as.factor(mice$sex)
mice$cage <- as.factor(mice$cage)

attach(mice)

########################################################################

#(b)



anova.tg <- lm(weight ~ tg *sex)
summary(anova.tg)
anova(anova.tg)


# Die Kombination aus tg und dem Geschlecht liefert ein brauchbares 
# Modell, besonders wenn es darum geht Anhand des Gewichts zu entscheiden, ob  
# männliche Mäuse am Down_Syndrom leiden
 

anova.cage <- lm(weight ~ cage)
summary(anova.cage)
anova(anova.cage)


# Aber auch Käfignummer
# allein erklären die Schwankungen in ausreichendem Maß, obwohl die Käfige in 
# Bezug auf tg eine heterogene Zusammensetzung besitzen. Das könnte darauf hindeuten,
# dass das Gewicht nicht wirklich von tg abhängt. Die Ergebnisse des models anova.tg
# also Scheineffekte sind.  







