#Lösung des neunten Übungsblattes
#Autoren: Bernd Klaus und Verena Zuber
#6. Januar 2010
########################################################################

 #(a) 
 library(DAAG)
 data<-toycars


 #(b) 
 data$car = as.factor(data$car)
 attach(data)
 #(c)
 boxplot(distance ~ car)

 #(d)
 lm.car1 <- lm(distance[which(car==1)]~angle[which(car==1)])
 summary(lm.car1)


 lm.car2 <- lm(distance[which(car==2)]~angle[which(car==2)])
 summary(lm.car2)

 lm.car3 <- lm(distance[which(car==3)]~angle[which(car==3)])
 summary(lm.car3)

 #(e) 
 par(mfcol=c(1,3))

 plot(angle[which(car==1)], distance[which(car==1)] )
 abline(lm.car1$coefficients, col = "red")

 plot(angle[which(car==2)], distance[which(car==2)] )
 abline(lm.car2$coefficients, col = "red")

 plot(angle[which(car==3)], distance[which(car==3)] )
 abline(lm.car3$coefficients, col = "red")

 #(f) 
 plot(lm.car1)
 plot(lm.car2)
 plot(lm.car3)
