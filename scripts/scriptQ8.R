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
## Question 8 : Stabilité du modèle
#******************************************************

borne = c("2003-01-01")
data1Journa = RentJ$Rt[RentJ$Dates < borne]
data2Journa = RentJ$Rt[RentJ$Dates > borne]

data1Hebdo = RentH$Rt[RentH$Dates < borne]
data2Hebdo = RentH$Rt[RentH$Dates > borne]


garch1 = fGarch::garchFit(formula = ~arma(1,1)+garch(1,2),data=data1Journa)
garch2 = fGarch::garchFit(formula = ~arma(1,1)+garch(1,2),data=data2Journa)

x = fGarch::predict(garch1, n.ahead = 4000)
y = fGarch::predict(garch2, n.ahead = 4000)
ks2Test(x$meanForecast,y$meanForecast)

garch1H = fGarch::garchFit(formula = ~arma(2,1)+garch(1,3),data=data1Hebdo)
garch2H = fGarch::garchFit(formula = ~arma(2,1)+garch(1,3),data=data2Hebdo)
x = fGarch::predict(garch1H, n.ahead = 4000)
y = fGarch::predict(garch2H, n.ahead = 4000)
ks2Test(x$meanForecast,y$meanForecast)
