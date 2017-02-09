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

