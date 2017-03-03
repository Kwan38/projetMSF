##########################################################
# Projet Méthodes Statistiques appliquées à la Finance

# Ruimy Benjamin
# Voong Kwan
# Ibakuyumcu Arnaud

# Script R utilisé pour répondre aux questions du Sujet
##########################################################

.libPaths("RLib/")
library(RSvgDevice)
library(MASS)
load("data/RentJAPDOWA.RData")

#******************************************************
## Question 5 : Corrélation des abs et carré des rentas
#******************************************************


absRentaPrec = abs(RentJ$Rt[-length(RentJ$Rt)])
absRentaSuiv = abs(RentJ$Rt[-1])

squareRentaPrec = RentJ$Rt[-length(RentJ$Rt)] ^2
squareRentaSuiv = RentJ$Rt[-1]^2

#Auto-Correl partielle (si on l'évalue en 1 on trouve le même resultat que ce que j'ai fait en manuel mais moins de decimales)

devSVG("images/Quest5/AutoCorrelationPartielleAbs.svg")
pacf(abs(RentJ$Rt))
dev.off()
devSVG("images/Quest5/AutoCorrelationPartielleSquare.svg")
pacf(RentJ$Rt^2)
dev.off()

#Légèrement corrélé, mais pas moyen de faire de stratégie car on se sait pas si cela va monter (si on a une renta positive) 
# ou si les rentas vont descendre (si on a une renta negative) 
#Sauf si on a des produits performances sur le Nikkei

cor(absRentaSuiv,absRentaPrec)
cor(squareRentaPrec,squareRentaSuiv)

