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
## Question 7 : Performance prédictive du modèle
#******************************************************


borne <- c("2014-01-01")
Renta = RentH$Rt[RentH$Dates < borne]
model = fGarch::garchFit(formula = ~arma(2,1)+garch(1,3),data = Renta);
nbDate = length(RentH$Rt)-length(Renta)
prediction = fGarch::predict(object = model,n.ahead = nbDate,plot = TRUE)
plot(x = RentH$Dates[(length(Renta)+1):length(RentH$Dates)],y = RentH$Rt[(length(Renta)+1):length(RentH$Dates)],type = "l")
plot(prediction$standardDeviation)
