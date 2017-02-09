##########################################################
# Projet Méthodes Statistiques appliquées à la Finance

# Ruimy Benjamin
# Voong Kwan
# Ibakuyumcu Arnaud

# Script R utilisé pour répondre aux questions du Sujet
##########################################################
.libPaths("RLib/")

library(MASS)
library(RSvgDevice)

load("data/RentJAPDOWA.RData")

#******************************************************
## Quelques graphiques supplémentaires
#******************************************************
print("")
print("###########################################")
print("############ TP MSF : Script R ############")
print("###########################################")
print("")
print("--------------------------------------------")
print(" 0) Graphique des rentabilités ")
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

print("--------> Graphique des rentabilités sauvegardé dans images/autres/")
print("")

#******************************************************
## Question 1 : Distribution empirique des rentabilités
#******************************************************


print("--------------------------------------------")
print(" 1) Répartition des rentabilités ")
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


print("----------> Répartitions des rentabilités sauvegardées dans images/Quest1")
print("")

#******************************************************
## Question 2 : Dépendance linéaire des rentabilités
#******************************************************

#on va essayer de regarder les covariances des rentabilités pour étudier leur dépendance en se plaçacant
#dans le modèle ARMA


library(tseries)
library(RRegArch)

RtH = RentH$Rt
RtJ = RentJ$Rt
RtM = RentM$Rt


#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités mensuelles
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités mensuelles")
devSVG("images/Quest2/acf_pacf_RentM")
split.screen(c(1,2))
screen(1)
acf(RentM$Rt, lag.max = 100)
screen(2)
pacf(RentM$Rt, lag.max = 300)
dev.off()
print("----------> Graphiques acf et pacf sauvegardées dans images/Quest2")
print("On observe sur ces graphiques que p + q = 7 + 1 = 8")

#définition du modèle ARMA(7,1)
armaNIKKEIMensu <- arma(RtM, lag=list(ar=c(1,7), ma=c(1)))

R2NIKKEIMensu <- 1 - var(armaNIKKEIMensu$residuals[!is.na(armaNIKKEIMensu$residuals)])/var(RtM)

print("R^2 = ")
R2NIKKEIMensu

#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités hebdomadaires
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités hebdomadaires")
acf(RtH, lag.max = 100)
print("on observe donc que q = 1 ou q = 2 a voir encore... et nous on prendra 1 pour l'instant")
pacf(RtH, lag.max = 100)
print("on observe donc que p = 5")

#définition du modèle ARMA(5,1)
armaNIKKEIhebdo <- arma(RtH, lag=list(ar=c(1,5), ma=c(1)))

R2NIKKEIhebdo <- 1 - var(armaNIKKEIhebdo$residuals[!is.na(armaNIKKEIhebdo$residuals)])/var(RtH)

print("R^2 = ")
R2NIKKEIhebdo


#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières")
acf(RtJ, lag.max = 100)
print("on observe donc que q = 1")
pacf(RtJ, lag.max = 300)
print("on observe donc que p = 14")

#définition du modèle ARMA(14,1)
armaNIKKEIjourna <- arma(RtJ, lag=list(ar=c(1,14), ma=c(1)))

R2NIKKEIjourna <- 1 - var(armaNIKKEIjourna$residuals[!is.na(armaNIKKEIjourna$residuals)])/var(RtJ)

print("R^2 = ")
R2NIKKEIjourna


print("Covariance des renta mensuelles : ")
cov(RtM,RtM)

print("Covariance des renta hebomadaires : ")
cov(RtH,RtH)

print("Covariance des renta journalières : ")
cov(RtJ,RtJ)










#******************************************************
## Question 3 : Étude du modèle de Markov HMM
#******************************************************











#******************************************************
## Question 4 : Espérance des rentabilités journalières
#******************************************************


print("--------------------------------------------")
print(" 4) Espérance des rentabilités journalières")
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

#Variation de l'espérance
print("Variation relative de l'espérance en pourcentage : ")
((mean(RentJPeriode2)-mean(RentJPeriode1))/(mean(RentJPeriode1)))*100
print("                **********   ")

#Calcul de la variance
print("Variance période [1990-2002]")
var(RentJPeriode1)
print("Variance période [2003-2016]")
var(RentJPeriode2)

#Variation de la variance
print("Variation relative de la variance en pourcentage : ")
((var(RentJPeriode2)-var(RentJPeriode1))/(var(RentJPeriode1)))*100







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
