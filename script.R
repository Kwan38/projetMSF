##########################################################
# Projet Méthodes Statistiques appliquées à la Finance

# Ruimy Benjamin
# Voong Kwan
# Ibakuyumcu Arnaud

# Script R utilisé pour répondre aux questions du Sujet
##########################################################

load("data/RentJAPDOWA.RData")
library(MASS)
library(RSvgDevice)

#******************************************************
## Question 1 : Distribution empirique des rentabilités
#******************************************************

#Affichage de l'histogramme, densité, boxplot, droite de henry
#RentaJ
devSVG("images/RepartitionJ.svg")
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
devSVG("images/RepartitionH.svg")
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
devSVG("images/RepartitionM.svg")
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
