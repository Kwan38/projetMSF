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
library(RHmm)

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
devSVG("images/autres/rentabilitesJ.svg")
plot(RentJ$Dates, RentJ$Rt,type="l", main="Rentabilité journalière",
     ylab="R", xlab="Dates")
dev.off()
devSVG("images/autres/rentabilitesH.svg")
plot(RentH$Dates, RentH$Rt,type="l", main="Rentabilité hebdomadaire",
     ylab="R", xlab="Dates")
dev.off()
devSVG("images/autres/rentabilitesM.svg")
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
densityRt <- density(RentJ$Rt)
normDens <- dnorm(densityRt$x, mean = mean(RentJ$Rt), sd = sd(RentJ$Rt))
hist(RentJ$Rt,freq=FALSE, main="RentaJ : Histogramme et densité",
     ylim=c(0,50), col="lightgreen", xlab="Rentabilité", ylab="Densité",
     breaks = seq(-0.17,0.17, len = 200))
lines(densityRt, col="darkgreen", lwd=2)
lines(densityRt$x,normDens, col="red")
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
densityRt <- density(RentH$Rt)
normDens <- dnorm(densityRt$x, mean = mean(RentH$Rt), sd = sd(RentH$Rt))
hist(RentH$Rt,freq=FALSE, main="RentaH : Histogramme et densité",
     ylim=c(0,30), col="lightgreen", xlab="Rentabilité", ylab="Densité",
     breaks = seq(-0.24,0.3, len = 200))
lines(densityRt, col="darkgreen", lwd=2)
lines(densityRt$x, normDens, col="red")
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
densityRt <- density(RentM$Rt)
normDens <- dnorm(densityRt$x, mean = mean(RentM$Rt), sd = sd(RentM$Rt))
hist(RentM$Rt,freq=FALSE, main="RentaM : Histogramme et densité",
     ylim=c(0,30), col="lightgreen", xlab="Rentabilité", ylab="Densité",
     breaks = seq(-0.3,0.2, len = 200))
lines(densityRt, col="darkgreen", lwd=2)
lines(densityRt$x,normDens, col="red")
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

#Information sur Kurtosys et Skewness
library(moments)
print("----------> Kurtosys et Skewness pour les rentabilités journalières : ")
print(list(kurt = kurtosis(RentJ$Rt),skew = skewness(RentJ$Rt)))
print("----------> Kurtosys et Skewness pour les rentabilités hebdomadaires : ")
print(list(kurt = kurtosis(RentH$Rt),skew = skewness(RentH$Rt)))
print("----------> Kurtosys et Skewness pour les rentabilités mensuelles : ")
print(list(kurt = kurtosis(RentM$Rt),skew = skewness(RentM$Rt)))

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










#******************************************************
## Question 3 : Étude du modèle de Markov HMM
#******************************************************


ResHMMFit <- HMMFit(RentJ$Rt, nStates=3)
ViterbiPath <- viterbi(ResHMMFit, RentJ$Rt)

# Graphic diagnostic
devSVG("images/Quest3/HMMGraphic.svg")
HMMGraphicDiag(ViterbiPath, ResHMMFit,RentJ$Rt)
dev.off()
devSVG("images/Quest3/HMMPlotSeries.svg")
HMMPlotSerie(RentJ$Rt, ViterbiPath, oneFig = TRUE)
dev.off()






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
