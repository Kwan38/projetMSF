##########################################################
# Projet Méthodes Statistiques appliquées à la Finance

# Ruimy Benjamin
# Voong Kwan
# Ibakuyumcu Arnaud

# Script R utilisé pour répondre aux questions du Sujet
##########################################################

library(MASS)
library(RSvgDevice)

load("data/RentJAPDOWA.RData")

#******************************************************
## Quelques graphiques supplémentaires
#******************************************************

print("############ TP MSF : Script R ############")
print("--------------------------------------------")
print(" Graphique des rentabilités ")
print("--------------------------------------------")
#Graphique des rentabilités
devSVG("images/autres/rentabilites.svg")
split.screen(c(2,1))
screen(1)
plot(RentJ$Dates, RentJ$Rt,type="l", main="Rentabilité journalière",
     ylab="R", xlab="Dates")
screen(2)
split.screen(c(1,2), screen(2))
plot(RentH$Dates, RentH$Rt,type="l", main="Rentabilité hebdomadaire",
     ylab="R", xlab="Dates")
screen(4)
plot(RentM$Dates, RentM$Rt,type="l", main="Rentabilité mensuelle",
     ylab="R", xlab="Dates")
dev.off()


#******************************************************
## Question 1 : Distribution empirique des rentabilités
#******************************************************


print("--------------------------------------------")
print(" Répartition des rentabilités ")
print("--------------------------------------------")

#Affichage de l'histogramme, densité, boxplot, droite de henry
#RentaJ
devSVG("images/Quest1/RepartitionJ.svg")
split.screen(c(2,1))
screen(1)
hist(RentJ$Rt,freq=FALSE, main="RentaJ : Histogramme et densité",
     ylim=c(0,50), col="lightgreen", xlab="Rentabilité", ylab="Densité")
lines(density(RentJ$Rt), col="red", lwd=2)
screen(2)
split.screen(c(1,2),screen(2))
boxplot(RentJ$Rt, main="Diagramme moustache")
screen(4)
qqnorm(RentJ$Rt, main = "droite de Henry", xlab="Quantiles théoriques", 
       ylab="Quantiles empiriques")
qqline(RentJ$Rt, col="red")
dev.off()
#RentaH
devSVG("images/Quest1/RepartitionH.svg")
split.screen(c(2,1))
screen(1)
hist(RentH$Rt,freq=FALSE, main="RentaH : Histogramme et densité",
     ylim=c(0,30), col="lightgreen", xlab="Rentabilité", ylab="Densité")
lines(density(RentH$Rt), col="red", lwd=2)
screen(2)
split.screen(c(1,2),screen(2))
boxplot(RentH$Rt, main="Diagramme moustache")
screen(4)
qqnorm(RentH$Rt, main = "droite de Henry", xlab="Quantiles théoriques", 
       ylab="Quantiles empiriques")
qqline(RentH$Rt, col="red")
dev.off()
#RentaM
devSVG("images/Quest1/RepartitionM.svg")
split.screen(c(2,1))
screen(1)
hist(RentJ$Rt,freq=FALSE, main="RentaM : Histogramme et densité",
     ylim=c(0,30), col="lightgreen", xlab="Rentabilité", ylab="Densité")
lines(density(RentM$Rt), col="red", lwd=2)
screen(2)
split.screen(c(1,2),screen(2))
boxplot(RentM$Rt, main="Diagramme moustache")
screen(4)
qqnorm(RentM$Rt, main = "droite de Henry", xlab="Quantiles théoriques", 
       ylab="Quantiles empiriques")
qqline(RentM$Rt, col="red")
dev.off()


#******************************************************
## Question 2 : Dépendance linéaire des rentabilités
#******************************************************











#******************************************************
## Question 3 : Étude du modèle de Markov HMM
#******************************************************











#******************************************************
## Question 4 : Espérance des rentabilités journalières
#******************************************************


print("--------------------------------------------")
print(" Espérance des rentabilités journalières")
print(" sur les périodes 1990-2002 et 2003-2016")
print("--------------------------------------------")

datesBornes <- Sys.Date()
datesBornes <- c("1990-01-01", "2002-01-01")
#Index de la période 1990-2002 : [4201;7405]
RentJPeriode1 <- RentJ$Rt[RentJ$Dates >= datesBornes[1] 
		       & RentJ$Dates < datesBornes[2]]
#Index de la période 2003-2016 : [7406;]
RentJPeriode2 <- RentJ$Rt[RentJ$Dates >= datesBornes[2]]

#Calcul de l'espérance
print("Espérance période [1990-2002]")
mean(RentJPeriode1)
print("Espérance période [2003-2016]")
mean(RentJPeriode2)





#******************************************************
## Question 5 : Corrélation des abs et carré des rentas
#******************************************************













#******************************************************
## Question 6 : Modélisation de type ARCH
#******************************************************













#******************************************************
## Question 7 : Performance prédictive du modèle
#******************************************************











#******************************************************
## Question 8 : Stabilité du modèle
#******************************************************
