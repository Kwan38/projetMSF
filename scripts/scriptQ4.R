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




