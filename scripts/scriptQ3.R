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
library(RHmm)
load("data/RentJAPDOWA.RData")

#******************************************************
## Question 3 : Étude du modèle de Markov HMM
#******************************************************



ResHMMFit <- HMMFit(RentJ$Rt, nStates=3)
ViterbiPath <- viterbi(ResHMMFit, RentJ$Rt)

# Graphic diagnostic
devSVG("images/Quest3/HMMGraphic.svg")
HMMGraphicDiag(ViterbiPath, ResHMMFit,RentJ$Rt)
dev.off()
devSVG("images/Quest3/HMMPlotSeries.svg")
HMMPlotSerie(RentJ$Rt, ViterbiPath, oneFig = TRUE)
dev.off()