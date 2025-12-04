library(MASS) 
library(dplyr)

################################################################################

#Pré-requis : Exécution du fichier Traitement/nettoyage.R pour générer data.csv 

# Ce script contient  : 
# 1) Le chargement de la table data
# 2) L'extraction des variables de stratification à conserver 
# 3) L'ajustement du modèle logistique complet
# 4) La sélection backward selon les critère AIC et BIC
# 5) L'extraction et la sauvegarde des variables retenues par la sélection

################################################################################

data <- read.csv("Data/data.csv")  

vars_obligatoires <- c("ARM_NUM", "INCL_SEPSIS_YN","INCL_AKIN","AGE_CLASS")

mod_full <- glm(PNEUMONIA_YN ~ .,
                data = data, family = binomial)

backward_aic <- stepAIC(mod_full, direction = "backward", trace = TRUE)

n <- nrow(data)
backward_bic <- stepAIC(mod_full, direction = "backward", k = log(n), trace = TRUE)

candidats_selec_aic <- attr(terms(backward_aic), "term.labels")
candidats_selec_bic <- attr(terms(backward_bic), "term.labels")

vars_backward_aic <- unique(c(candidats_selec_aic, vars_obligatoires))

vars_backward_bic <- unique(c(candidats_selec_bic, vars_obligatoires))

save(vars_backward_aic, file = "Selection_variables/vars_backward_aic.RData")
save(vars_backward_bic, file = "Selection_variables/vars_backward_bic.RData")
