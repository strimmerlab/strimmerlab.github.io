#Lösung des zwölften Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#12. Januar 2010
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








