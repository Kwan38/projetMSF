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

#On va maintenant modéliser plusieurs types de modèles ARCH : 

print("*** Modèle ARCH(3), algo : nlminb ***")
archMod <- fGarch::garchFit(formula = ~arma(2,1) + garch(3,0),data = RentJ$Rt, algorithm = 'nlminb')
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80) 
print("R² = ")
1 - (var(archMod@residuals)/var(RentJ$Rt))
qqnormPlot(archMod@residuals)

print("Pas de convergence. Résidus qui ne sont pas normaux.... R² = -0.0027444. AIC = -6.09; BIC = -6.09")

print("*** Modèle ARCH(4), algo : lbfgsb ***")
archMod <- fGarch::garchFit(formula = ~arma(2,1) + garch(4,0),data = RentJ$Rt, algorithm = 'lbfgsb')
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80)
print("R² = ")
1 - (var(archMod@residuals)/var(RentJ$Rt))
qqnormPlot(archMod@residuals)

print("Pas de convergence. Résidus qui ne sont pas normaux.... R² =-0.00460867. AIC =-6.131549; BIC = -6.125424")

print("*** Modèle GARCH(1,1), algo : lbfgsb ***")
archMod <- fGarch::garchFit(formula = ~arma(1,1) + garch(1,1),data = RentJ$Rt, algorithm = 'lbfgsb')
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80)
print("R² = ")
1 - (var(archMod@residuals)/var(RentJ$Rt))
qqnormPlot(archMod@residuals)

print("Convergence. Résidus qui ne sont pas normaux.... R² =-0.003092868. AIC = -6.225940; BIC = -6.221857")

print("*** Modèle GARCH(1,4), algo : lbfgsb ***")
archMod <- fGarch::garchFit(formula = ~arma(1,1) + garch(1,4),data = RentJ$Rt, algorithm = 'lbfgsb')
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80)
print("R² = ")
1 - (var(archMod@residuals)/var(RentJ$Rt))
qqnormPlot(archMod@residuals)

print("Convergence. Résidus qui ne sont pas normaux.... R² = -0.003059086. AIC = -6.228155; BIC = -6.222031")


print("*** Modèle GARCH(2,4), algo : lbfgsb ***")
archMod <- fGarch::garchFit(formula = ~arma(4,4) + garch(2,4),data = RentH$Rt, algorithm = 'nlminb')
fGarch::summary(archMod)
acJ <- stats::acf(x = archMod@residuals, lag.max = 80)
pacJ <- stats::pacf(x = archMod@residuals, lag.max = 80)
print("R² = ")
1 - (var(archMod@residuals)/var(RentH$Rt))
qqnormPlot(archMod@residuals)
archMod@fit$ics[1]
print("Convergence. Résidus qui ne sont pas normaux.... R² = -0.003077218. AIC = -6.225720; BIC = -6.220957")

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

tmpMinAIC
tmpMaxR 

tmpMaxi1
tmpMaxj1

tmpMaxi2
tmpMaxj2



