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

