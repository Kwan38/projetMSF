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
library(fGarch)

load("data/RentJAPDOWA.RData")

#******************************************************
## Question 6 : Modélisation de type ARCH
#******************************************************

#On modélise plusieurs types de modèles ARCH : 

print("*** Modèle GARCH(1,3), Journa***")
archMod <- fGarch::garchFit(formula = ~arma(1,1) + garch(1,2),data = RentJ$Rt)
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80) 
print("R² = ")
1 - (var(archMod@residuals)/var(RentJ$Rt))
qqnormPlot(archMod@residuals)
archMod@fit$ics[1] 


print("*** Modèle GARCH(1,3), Hebdo ***")
archMod <- fGarch::garchFit(formula = ~arma(2,1) + garch(1,3),data = RentH$Rt)
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80) 
print("R² = ")
1 - (var(archMod@residuals)/var(RentH$Rt))
qqnormPlot(archMod@residuals)
archMod@fit$ics[1] 

tmpMinAIC = 0
tmpMaxR = 0
tmpMaxi1 = 0
tmpMaxj1 = 0

tmpMaxi2 = 0
tmpMaxj2 = 0
for(i1 in 0:4){
  for(j1 in 0:4){
    for(i2 in 1:4){
      for(j2 in 0:4){
        print("new")
        print(i1)
        print(j1)
        print(i2)
        print(j2)
        formula = as.formula(paste("~arma(",i1,",",j1,") + garch(",i2,",",j2,")"))
        archMod <- fGarch::garchFit(formula = formula,data = RentJ$Rt, algorithm = 'nlminb',trace = FALSE)
               #fGarch::summary(archMod)
        acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
        pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80)
        RSquare = 1 - (var(archMod@residuals)/var(RentJ$Rt))
        if(-(archMod@fit$ics[1] - tmpMinAIC) > 0.001){
          tmpMinAIC = archMod@fit$ics[1]
          tmpMaxR = RSquare
          tmpMaxi1 = i1; 
          tmpMaxj1 = j1; 
          tmpMaxi2 = i2; 
          tmpMaxj2 = j2; 
        }else if((-archMod@fit$ics[1] + tmpMinAIC) > 0.0){
          
          if(RSquare > tmpMaxR){
            
            tmpMinAIC = archMod@fit$ics[1]
            tmpMaxR = RSquare
            tmpMaxi1 = i1; 
            tmpMaxj1 = j1; 
            tmpMaxi2 = i2; 
            tmpMaxj2 = j2; 
            
          } 
          
        }
        
        
      }
      }
    }
    
}

