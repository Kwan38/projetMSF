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

datesBornes <- c("1990-01-01", "2002-12-31")
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


#test t pour vérifier si les moyennes sont significativement différentes
#---> Interprétation, regarder si les moyennes sont dans l'interval de confiance + regarder si p-valeur petite (--> on aimerait rejetter que c'est égal 
# p-val un tout petit peu trop grande et on se trouve vraiment au bord de l'intervalle de confiance)
# Cas critique
t.test(RentJPeriode1,RentJPeriode2)


# Graphic diagnostic
devSVG("images/Quest4/DensityGraphic.svg")
densityPeriode2 = density(RentJPeriode2);

plot(densityPeriode2,col="red",main = "Densité des périodes 1990-2002 (bleu) et 2003-2016 (rouge)")
densityPeriode1 = density(RentJPeriode1);
lines(densityPeriode1,col="blue")
dev.off()



#Calcul de la variance
print("Variance période [1990-2002]")
var(RentJPeriode1)
print("Variance période [2003-2016]")
var(RentJPeriode2)

#Variation de la variance
print("Variation relative de la variance en pourcentage : ")
((var(RentJPeriode2)-var(RentJPeriode1))/(var(RentJPeriode1)))*100

#On a une valeur de ratio de variance qui est en pleins dans l'intervalle de confiance, et une p-valeur grande, 
#Du coup on ne peut pas rejeter H0 (le fait qu'elles sont pareils), et la position dans l'intervalle de confiance nous dit qu'elle ne sont pas 
#Significativement différentes ("En gros")
var.test(RentJPeriode1, RentJPeriode2)
