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
## Quelques graphiques supplémentaires
#******************************************************

source("scripts/scriptQ0.R")

#******************************************************
## Question 1 : Distribution empirique des rentabilités
#******************************************************

source("scripts/scriptQ1.R")


#******************************************************
## Question 2 : Dépendance linéaire des rentabilités
#******************************************************


source("scripts/scriptQ2.R")


#******************************************************
## Question 3 : Étude du modèle de Markov HMM
#******************************************************


source("scripts/scriptQ3.R")


#******************************************************
## Question 4 : Espérance des rentabilités journalières
#******************************************************


source("scripts/scriptQ4.R")


#******************************************************
## Question 5 : Corrélation des abs et carré des rentas
#******************************************************


source("scripts/scriptQ5.R")


#******************************************************
## Question 6 : Modélisation de type ARCH
#******************************************************


source("scripts/scriptQ6.R")


#******************************************************
## Question 7 : Performance prédictive du modèle
#******************************************************


source("scripts/scriptQ7.R")


#******************************************************
## Question 8 : Stabilité du modèle
#******************************************************


source("scripts/scriptQ8.R")
