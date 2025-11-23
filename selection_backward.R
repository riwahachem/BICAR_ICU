library(stats)
library(MASS) 
library(dplyr)
library(car)

data <- read.csv("data.csv")  

#variables à conserver obligatoirement
vars_obligatoires = c("INCL_SEPSIS_YN","INCL_AKIN","ARM_NUM", "AGE_CLASS", "SEX", "BMI", "SOFA")

#variables candidates à éliminer (toutes sauf cible et obligatoires)
candidats <- setdiff(names(data), c("PNEUMONIA_YN", vars_obligatoires))

# Formule de départ avec candidats non forcés uniquement
#cette ligne transforme c("X1","X2") en P_YN ~ X1+X2 (pour pouvoir appliquer glm ensuite)
formule_init <- as.formula(paste("PNEUMONIA_YN ~", paste(candidats, collapse = " + ")))

# Modèle de départ avec toutes les variables candidates
mod_init <- glm(formule_init, data = data, family = binomial)

# Selection backward avec critère AIC, suppression d'une variable à la fois pour voir si l'AIC diminue
#Fonctionnement stepAIC : 
# 1) calcul l'AIC avec toutes les variables
# 2) calcul l'AIC avec 1 variables en moins : si l'AIC a diminué on supprime, sinon on garde
# 3) on réitère en testant la suppression de chacune des variables une par une
# 4) on s'arrête quand aucune suppression ne réduit l'AIC

backward_aic <- stepAIC(mod_init, direction = "backward", trace = TRUE)

n <- nrow(data)
backward_bic <- stepAIC(mod_init, direction = "backward", k = log(n), trace = TRUE)

#Variables selectionnées parmi les variables candidates
candidats_selec_aic <- attr(terms(backward_aic), "term.labels")
candidats_selec_bic <- attr(terms(backward_bic), "term.labels")

#Variables finales
vars_backward_aic <- c(candidats_selec_aic, vars_obligatoires)
vars_backward_bic <- c(candidats_selec_bic, vars_obligatoires)

# Modèle final à utiliser pour la régression logistique
formule_finale_aic <- as.formula(paste("PNEUMONIA_YN ~", paste(vars_backward_aic, collapse = " + ")))
formule_finale_bic <- as.formula(paste("PNEUMONIA_YN ~", paste(vars_backward_bic, collapse = " + ")))

final_model_bic <- glm(formule_finale_bic, data = data, family = binomial)
final_model_aic <- glm(formule_finale_aic, data = data, family = binomial)

# Affichage des variables retenues
print(vars_backward_aic)
print(vars_backward_bic)

save(vars_backward_aic, file = "vars_backward_aic.RData")
save(vars_backward_bic, file = "vars_backward_bic.RData")
