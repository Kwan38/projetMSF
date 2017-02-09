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
## Question 2 : Dépendance linéaire des rentabilités
#******************************************************

#on va essayer de regarder les covariances des rentabilités pour étudier leur dépendance en se plaçacant
#dans le modèle ARMA


.libPaths("/user/1/taram/Public/RLib/")
library(tseries)
library(RRegArch)

RtH = RentH$Rt
RtJ = RentJ$Rt
RtM = RentM$Rt


#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités mensuelles
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités mensuelles")
devSVG("images/Quest2/acf_pacf_RentM.svg")
split.screen(c(1,2))
screen(1)
acf(RentM$Rt, lag.max = 100)
screen(2)
pacf(RentM$Rt, lag.max = 300)
dev.off()
print("----------> Graphiques acf et pacf sauvegardées dans images/Quest2")
print("On observe sur ces graphiques que p + q = 7 + 1 = 8")

#définition du modèle ARMA(7,1)
armaRtM <- arma(RentM$Rt, lag=list(ar=c(1,7), ma=c(1)))
R2RtM <- 1 - var(armaRtM$residuals[!is.na(armaRtM$residuals)])/var(RentM$Rt)

print("R^2 = ")
R2RtM

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

