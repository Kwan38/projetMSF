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

library(tseries)
library(RRegArch)
library(TSA)

#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités hebdomadaires
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités hebdomadaires")
devSVG("images/Quest2/acf_pacf_RentH.svg")
split.screen(c(1,2))
screen(1)
stats::acf(RentH$Rt, lag.max = 80)
screen(2)
stats::pacf(RentH$Rt, lag.max = 80)
dev.off()
print("----------> Graphiques acf et pacf sauvegardées dans images/Quest2")
print("On observe sur ces graphiques que p + q = 8 + 1 = 9")
print("")
print("La librairie TSA nous donne les combinaisons (p,q) signifiantes : ")
eacfMat <-TSA::eacf(RentH$Rt, ar.max = 8, ma.max = 5)
eacfMat

#choix du modèle AR/MA (celui ayant le plus petit critère d'AIC)
AICMat <- matrix(0,nrow = 8,ncol = 5)
for (i in 1:8) {
  for (j in 1:5) {
    if (i != 0 || j != 0){
      if (eacfMat$symbol[i,j]=="x") {
        armaRtHSum <- summary(arma(RentH$Rt, order = c(i-1,j-1)))
        AICMat[i,j] = armaRtHSum$aic
      }else{
        AICMat[i,j] = "x"
      }
    }
  }
}
R2RtH <- 1 - var(armaRtH$residuals[!is.na(armaRtH$residuals)])/var(RentH$Rt)

print("R^2 = ")
R2RtH

#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières")
devSVG("images/Quest2/acf_pacf_RentJ.svg")
split.screen(c(1,2))
screen(1)
stats::acf(RentJ$Rt, lag.max = 80)
screen(2)
stats::pacf(RentJ$Rt, lag.max = 80)
dev.off()
print("On observe sur ces graphiques que p + q = 2 + 1 = 3")
print("")
print("La librairie TSA nous donne les combinaisons (p,q) signifiantes : ")
TSA::eacf(RentJ$Rt, ar.max = 2, ma.max = 2)

print("Covariance des renta mensuelles : ")
cov(RtM,RtM)

print("Covariance des renta hebomadaires : ")
cov(RtH,RtH)

print("Covariance des renta journalières : ")
cov(RtJ,RtJ)
