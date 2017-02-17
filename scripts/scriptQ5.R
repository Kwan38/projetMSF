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
## Question 5 : Corrélation des abs et carré des rentas
#******************************************************


absRentaPrec = abs(RentJ$Rt[-length(RentJ$Rt)])
absRentaSuiv = abs(RentJ$Rt[-1])

squareRentaPrec = RentJ$Rt[-length(RentJ$Rt)] ^2
squareRentaSuiv = RentJ$Rt[-1]^2


#Légèrement corrélé, mais pas moyen de faire de stratégie car on se sait pas si cela va monter (si on a une renta positive) 
# ou si les rentas vont descendre (si on a une renta negative) 
cor(absRentaSuiv,absRentaPrec)
cor(squareRentaPrec,squareRentaSuiv)

