#Lösung des elften Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#5. Januar 2012
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

 pdf(file="BoxpPlotMeat.pdf", width=12, height=6)
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


new.factor<-rep(NA, length(mice$tg))
new.factor[which(mice$tg==0&mice$sex==0)]<-0
new.factor[which(mice$tg==0&mice$sex==1)]<-1
new.factor[which(mice$tg==1&mice$sex==0)]<-2
new.factor[which(mice$tg==1&mice$sex==1)]<-3


new.factor<-factor(new.factor, levels=c(0,1,2,3), labels=c("nonTG female","nonTG male","TG female","TG male"))

boxplot(weight~new.factor)



#(c)

anova.tg <- lm(weight ~  tg)
summary(anova.tg)
anova(anova.tg)


# Starker univariater Effekt der genetischen Veränderung auf das Gewicht.


anova.all <- lm(weight ~  tg*sex)
summary(anova.all)
anova(anova.all)



# ABER dieser Effekt verschwindet unter der Aufnahme des Geschlechts in das Modell!
# Denn männliche Mäuse sind immer schwerer als weibliche
# Interaktion ist signifikant weil die Gewichtsdifferenz bei nicht transgenetischen Mäusen
# weniger stark ausgeprägt ist als bei transgenetischen.






