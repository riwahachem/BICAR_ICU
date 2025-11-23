library(stats)
library(MASS) 
library(dplyr)
library(car)

data = read.csv("data.csv", header = TRUE, sep = " ")

#variables à conserver obligatoirement
vars_obligatoires = c("INCL_SEPSIS_YN","INCL_AKIN","ARM_NUM", "AGE", "SEX", "BMI", "SOFA")

#variables candidates à éliminer (toutes sauf cible et obligatoires)
candidats <- setdiff(names(data), c("PNEUMONIA_YN", vars_obligatoires))

# Formule de départ avec candidats non forcés uniquement
#cette ligne transforme c("X1","X2") en P_YN ~ X1+X2 (pour pouvoir appliquer glm ensuite)
formule_init <- as.formula(paste("PNEUMONIA_YN ~", paste(candidats, collapse = " + ")))

# Point de départ avec toutes les variables candidates
mod_init <- glm(formule_init, data = data, family = binomial)

# Selection backward avec critère AIC, suppression d'une variable à la fois pour voir si l'AIC diminue
#Fonctionnement stepAIC : 
# 1) calcul l'AIC avec toutes les variables
# 2) calcul l'AIC avec 1 variables en moins : si l'AIC a diminué on supprime, sinon on garde
# 3) on réitère en testant la suppression de chacune des variables une par une
# 4) on s'arrête quand aucune suppression ne réduit l'AIC

backward_aic <- stepAIC(mod_init, direction = "backward", trace = TRUE)

#Variables selectionnées parmi les variables candidates
candidats_selec <- attr(terms(backward_aic), "term.labels")

# Modèle final en ajoutant les variables forcées
vars_finales <- c(candidats_selec, vars_obligatoires)
formule_finale <- as.formula(paste("PNEUMONIA_YN ~", paste(vars_finales, collapse = " + ")))

# Modèle final (fit ultérieurement pour interprétation/prédiction)
final_model <- glm(formule_finale, data = data, family = binomial)

# Variables retenues
print(vars_finales)
