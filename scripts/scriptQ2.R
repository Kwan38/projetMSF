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
############### Partie hebdomadaire #####################
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

#choix du modèle AR/MA (celui ayant le plus petit critère d'AIC, plus grand R)
AICMat <- matrix(0,nrow = 3,ncol = 3)
R2Mat <- matrix(0,nrow = 3,ncol = 3)
# Il y a des problèmes d'optimisations (pour le fit) dès que p>3 et q>3 
for (i in 1:3) {
  for (j in 1:3) {
    if(i != 1 || j != 1){
      if (eacfMat$symbol[i,j]=="x") {
        armaRtH <- arma(RentH$Rt, order = c(i-1,j-1))
        #print(summary(armaRtH))
        AICMat[i,j] = summary(armaRtH)$aic
        R2Mat[i,j] = 1 - var(armaRtH$residuals[!is.na(armaRtH$residuals)])/var(RentH$Rt)
      }else{
        AICMat[i,j] = "_"
        R2Mat[i,j] = "_"
      }
    }
  }
}
print("Matrice d'AIC : ")
print(AICMat)
print("Matrice des R2 : ")
print(R2Mat)
print("On remarque que le modèle au AIC minimum, au R2 maximum est ARMA(2,1)")
armaRtH <- arma(RentH$Rt, order = c(2,1))
print(summary(armaRtH))
print("")
print("Chaque coefficient est significatif")
#Calcul du R2
R2RtH <- 1 - var(armaRtH$residuals[!is.na(armaRtH$residuals)])/var(RentH$Rt)
print("R^2 = ")
print(R2RtH)

#Étude des résidus
devSVG("images/Quest2/histResH.svg")
hist(armaRtH$residuals, breaks = seq(-0.24,0.18, len = 200), probability = T, 
     main = "Histogram of residuals", xlab = "Residuals", col= 'grey')
moy <- mean(armaRtH$residuals[!is.na(armaRtH$residuals)])
std <- sd(armaRtH$residuals[!is.na(armaRtH$residuals)])
plot(function(x) dnorm(x,moy,std), xlim = c(-0.2,0.2), add = TRUE,
     col = 'red', lwd = 1.5)
dev.off()
#Diagramme moustache et diagramme quantile quantile
devSVG("images/Quest2/moreRehH.svg")
split.screen(c(1,2))
screen(1)
fBasics::qqnormPlot(armaRtH$residuals[!is.na(armaRtH$residuals)], col = 'darkgreen')
screen(2)
boxplot(armaRtH$residuals, col = 'lightgreen', border = 'darkgreen', 
        main = "Diagramme moustache des résidus")
dev.off()

######################## Partie Journaliere ##############################
#recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières
print("Recherche des valeurs de p et q du modele ARMA(p,q) pour les rentabilités journalières")
devSVG("images/Quest2/acf_pacf_RentJ.svg")
split.screen(c(1,2))
screen(1)
stats::acf(RentJ$Rt, lag.max = 80)
screen(2)
stats::pacf(RentJ$Rt, lag.max = 80)
dev.off()
print("On observe sur ces graphiques que p + q = 6 + 1 = 7")
print("")
print("La librairie TSA nous donne les combinaisons (p,q) signifiantes : ")
eacfMat <- TSA::eacf(RentJ$Rt, ar.max = 5, ma.max = 5)
#choix du modèle AR/MA (celui ayant le plus petit critère d'AIC, plus grand R)
AICMat <- matrix(0,nrow = 5,ncol = 5)
R2Mat <- matrix(0,nrow = 5,ncol = 5)
for (i in 1:5) {
  for (j in 1:5) {
    if(i != 1 || j != 1){
      if (eacfMat$symbol[i,j]=="x") {
        armaRtJ <- arma(RentJ$Rt, order = c(i-1,j-1))
        #print(summary(armaRtH))
        AICMat[i,j] = summary(armaRtJ)$aic
        R2Mat[i,j] = 1 - var(armaRtJ$residuals[!is.na(armaRtJ$residuals)])/var(RentJ$Rt)
      }else{
        AICMat[i,j] = "_"
        R2Mat[i,j] = "_"
      }
    }
  }
}
print(AICMat)
print(R2Mat)
print("On remarque que le modèle au AIC minimum, au R2 maximum est ARMA(2,0)")
armaRtJ <- arma(RentJ$Rt, order = c(2,0))
print(summary(armaRtJ))
print("")
#Calcul du R2
R2RtJ <- 1 - var(armaRtJ$residuals[!is.na(armaRtJ$residuals)])/var(RentJ$Rt)
print("R^2 = ")
print(R2RtJ)

#Étude des résidus
devSVG("images/Quest2/histResJ.svg")
hist(armaRtJ$residuals, breaks = seq(-0.164,0.132, len = 200), probability = T, 
     main = "Histogram of residuals", xlab = "Residuals", col= 'grey')
moy <- mean(armaRtJ$residuals[!is.na(armaRtJ$residuals)])
std <- sd(armaRtJ$residuals[!is.na(armaRtJ$residuals)])
plot(function(x) dnorm(x,moy,std), xlim = c(-0.2,0.2), add = TRUE,
     col = 'red', lwd = 1.5)
dev.off()
#Diagramme moustache et diagramme quantile quantile
devSVG("images/Quest2/moreRehJ.svg")
split.screen(c(1,2))
screen(1)
fBasics::qqnormPlot(armaRtJ$residuals[!is.na(armaRtJ$residuals)], col = 'darkgreen')
screen(2)
boxplot(armaRtJ$residuals, col = 'lightgreen', border = 'darkgreen', 
        main = "Diagramme moustache des résidus")
dev.off()