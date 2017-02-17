#Première étape: charger le fichier des rentabilités
#Le dossier courant doit être celui qui contient les données

#####################################################################
# ATTENTION : Il est possible que certains doivent être installé avec 
# 			  l'interface de de RStudios
#####################################################################

library("moments")
install.packages("moments")

install.packages("stabledist");
#library("fBasics")
install.packages("fBasics")
install.packages("TSA")
install.packages("aod")

load("RentHNGKNGI.RData")
.libPaths("/user/1/taram/Public/RLib/");

library("tseries");
library("fGarch");
library("RHmm");

#Question 1:

#Etude des rentabilités journalières
plot(RentJ$Dates, RentJ$Rt,  type='l', col='red')

#Etude des rentabilités hebdomadaires
plot(RentH$Dates, RentH$Rt,  type='l', col='blue')

#Etude des rentabilités mensuelles
plot(RentM$Dates, RentM$Rt,  type='l', col='green')


hJ = sd(RentJ$Rt)/ length(RentJ$Rt)^(1/5);
hH = sd(RentH$Rt)/ length(RentH$Rt)^(1/5);
hM = sd(RentM$Rt)/ length(RentM$Rt)^(1/5);


# Pour le cas journalier
densityJ = density(RentJ$Rt, bw=hJ, kernel="gaussian");
normJ = dnorm(densityJ$x, mean = mean(RentJ$Rt) , sd = sd(RentJ$Rt));

plot(densityJ, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE,col='blue');
lines(densityJ$x,normJ,col='red');

fBasics::stableFit(RentJ$Rt, alpha = 1.60, beta = 0, gamma = 0, delta = 0,doplot=FALSE)
levyJ = stabledist::dstable(densityJ$x, alpha = 1.4700000000, beta=-0.0850000000, gamma = 0.0079098664, delta = 0.0007491609);
lines(densityJ$x,levyJ,col='green');


# Pour le cas hebdo 
densityH = density(RentH$Rt, bw=hH, kernel="gaussian");
normH = dnorm(densityH$x, mean = mean(RentH$Rt) , sd = sd(RentH$Rt));
plot(densityH, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE,col='blue');
lines(densityH$x,normH,col='red');

fBasics::stableFit(RentH$Rt, alpha = 1.60, beta = 0, gamma = 0, delta = 0,doplot=FALSE);
levyH = stabledist::dstable(densityH$x, 1.67700, beta=-0.0920, gamma = 0.020978498, delta = 0.003347667);
lines(densityH$x,levyH,col='green');

#Pour le cas mensuel 
densityM = density(RentM$Rt, bw=hM, kernel="gaussian");
normM = dnorm(densityM$x, mean = mean(RentM$Rt) , sd = sd(RentM$Rt));
plot(densityM, main = NULL, xlab = NULL, ylab = "Density", type = "l",
     zero.line = TRUE,col='blue');
lines(densityM$x,normM,col='red')

fBasics::stableFit(RentM$Rt, alpha = 1.60, beta = 0, gamma = 0, delta = 0,doplot=FALSE);
levyM = stabledist::dstable(densityM$x, alpha=1.64200000, beta=-0.3280, gamma = 0.04481532, delta = 0.01145903);
lines(densityM$x,levyM,col='green')


moments::skewness(RentJ$Rt)
moments::skewness(RentH$Rt)
moments::skewness(RentM$Rt)
moments::kurtosis(RentJ$Rt)
moments::kurtosis(RentH$Rt)
moments::kurtosis(RentM$Rt)



acJ <- acf(abs(RentJ$Rt), type = "correlation", plot = T)
pacJ <- acf(abs(RentJ$Rt), type = "partial", plot = T)


#Première étape: charger le fichier des rentabilités
#Le dossier courant doit être MSF

load("RentHNGKNGI.RData")
.libPaths("/user/1/taram/Public/RLib/");

#Deuxième étape: charger les librairies suivantes
.libPaths("/user/1/taram/Public/RLib/fGarch")
.libPaths("/user/1/taram/Public/RLib/RHmm")
.libPaths("/user/1/taram/Public/RLib/tseries")
.libPaths("/user/1/taram/Public/RLib/stabledlist")
.libPaths("/user/1/taram/Public/RLib/timeSeries")
install.packages('moments')
library(moments)

library("MASS", lib.loc ="/user/1/taram/Public/RLib")
library("nlme", lib.loc ="/user/1/taram/Public/RLib")
library("RHmm", lib.loc ="/user/1/taram/Public/RLib")





#Question 2

#Etude des rentabilités journalières
eacfJ = TSA::eacf(RentJ$Rt, ar.max=15, ma.max=15)
r <- tseries::arma(RentJ$Rt, order  = c(2, 4))
summary(r)
R <- 1 - var(r$residuals[10:6135]) / var(RentJ$Rt)
R
#  --Plot
hist(r$residuals ,breaks=seq(-0.15,0.18,len = 200),ylim=c(0,40),proba=T)
moy <- mean(r$residuals[5:6135])
var<- var(r$residuals[5:6135])
std<- sd(r$residuals[5:6135])
lim <- c(-0.4, 0.4)
plot(function(x) dnorm(x,moy,std), xlim = lim, add = TRUE, col = 'green')
fBasics::qqnormPlot(r$residuals[5:6135])

#Etude des rentabilités hebdomadaires
eacfH = TSA::eacf(RentH$Rt, ar.max=15, ma.max=15)
r <- tseries::arma(RentH$Rt, order  = c(3, 1))
summary(r)
R <- 1 - var(r$residuals[5:6135]) / var(RentH$Rt)
R
#  --Plot
hist(r$residuals ,breaks=seq(-0.35,0.25,len = 200),ylim=c(0,15),proba=T)
moy <- mean(r$residuals[4:1295])
std <- sd(r$residuals[4:1295])
lim <- c(-0.4, 0.4)
plot(function(x) dnorm(x,moy,std), xlim = lim, add = TRUE, col = 'red')
fBasics::qqnormPlot(r$residuals[5:1295])




# Question 3

#Fits an 3 states gaussian model
ResFit <- HMMFit(RentJ$Rt, nStates=3)
VitPath <- viterbi(ResFit, RentJ$Rt)
summary(ResFit)
summary(VitPath)

# Graphic diagnostic
HMMGraphicDiag(VitPath, ResFit,RentJ$Rt)
HMMPlotSerie(RentJ$Rt, VitPath, oneFig = TRUE)




# Question 4

x <- RentJ$Rt[1:2967]
y <- RentJ$Rt[2967:6135]
mx <- mean(x)
my <- mean(y)
varx <- var(x)
vary <- var(y)
mx
my
varx
vary

t.test(RentJ$Rt[1:3000], RentJ$Rt[3001:6000])
var.test(RentJ$Rt[1:3000], RentJ$Rt[3001:6000])
chisq.test(RentJ$Rt[1:3000], RentJ$Rt[3001:6000])

# Question 5
########## ARCH ########
# Journalier
r = tseries::arma(RentJ$Rt, order = c(3,5))
resJ = r$residuals[6:6126]^2
ga5 = fGarch::garchFit(formula = ~garch(4,0),data=resJ);

# Question 6
# Répéter cela pour différents modèles
mod = fGarch::garchFit(formula = ~arma(2,4)+garch(2,1),data=RentJ$Rt);
fGarch::summary(mod);
acJ <- acf(mod@residuals, type = "correlation", plot = T)
pacJ <- acf(mod@residuals, type = "partial", plot = T)
1-(var(mod@residuals)/var(RentJ$Rt))
qqnormPlot(mod@residuals)

mod = fGarch::garchFit(formula = ~arma(1,1)+garch(1,1),data=RentH$Rt);
fGarch::summary(mod);
acJ <- acf(mod@residuals, type = "correlation", plot = T)
pacJ <- acf(mod@residuals, type = "partial", plot = T)
1-(var(mod@residuals)/var(RentH$Rt))
qqnormPlot(mod@residuals)

#Question 7
mod7 = fGarch::garchFit(formula = ~arma(2,4)+garch(2,1),data=RentJ$Rt[1:5440]);
nbDate = length(RentJ$Rt)-5440;
pr7 = fGarch::predict(object = mod7,n.ahead = nbDate)
plot(x = RentJ$Dates[5441:length(RentJ$Dates)],y = RentJ$Rt[5441:length(RentJ$Dates)],type = "l")
lines(RentJ$Dates[5441:length(RentJ$Dates)],pr7$upperInterval,col='blue')
lines(RentJ$Dates[5441:length(RentJ$Dates)],pr7$lowerInterval,col='green')


mod72 = fGarch::garchFit(formula = ~arma(1,1)+garch(1,1),data=RentH$Rt[1:626]);
nbDate = length(RentH$Rt)-626;
pr72 = fGarch::predict(object = mod7,n.ahead = nbDate)
plot(x = RentH$Dates[627:length(RentJ$Dates)],y = RentH$Rt[627:length(Renth$Dates)],type = "l")
lines(RentH$Dates[627:length(RentJ$Dates)],pr72$upperInterval,col='blue')
lines(RentH$Dates[627:length(RentJ$Dates)],pr72$lowerInterval,col='green')

#Question 8
#installer aod

mod81 = fGarch::garchFit(formula = ~arma(2,4)+garch(2,1),data=RentJ$Rt[1:2967]);
mod82 = fGarch::garchFit(formula = ~arma(2,4)+garch(2,1),data=RentJ$Rt[2968:length(RentJ$Rt)])

x = fGarch::predict(mod81, n.ahead = 4000)
y = fGarch::predict(mod82, n.ahead = 4000)
ks2Test(x$meanForecast,y$meanForecast)

mod81 = fGarch::garchFit(formula = ~arma(1,1)+garch(1,1),data=RentH$Rt[1:627]);
mod82 = fGarch::garchFit(formula = ~arma(1,1)+garch(1,1),data=RentH$Rt[628:length(RentH$Rt)])
x = fGarch::predict(mod81, n.ahead = 4000)
y = fGarch::predict(mod82, n.ahead = 4000)
ks2Test(x$meanForecast,y$meanForecast)


