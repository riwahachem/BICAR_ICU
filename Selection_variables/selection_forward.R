library(MASS) 

################################################################################

#Pré-requis : Exécution du fichier Traitement/nettoyage.R pour générer data.csv 

# Ce script contient  : 
# 1) Le chargement de la table data
# 2) L'ajustement du modèle logistique vide avec seulement les variables de stratification
# 3) L'ajustement du modèle logistique complet
# 4) La sélection forward selon les critères AIC et BIC
# 5) L'extraction et la sauvegarde des variables retenues par la sélection

################################################################################

data <- read.csv("Data/data.csv")  

null_model <- glm( PNEUMONIA_YN ~ ARM_NUM + INCL_SEPSIS_YN + INCL_AKIN + AGE_CLASS,
                   data = data,family = binomial)

full_model <- glm(PNEUMONIA_YN ~ ., data = data, family = binomial)

forward_aic <- stepAIC(null_model,scope = list(lower = formula(null_model), upper  = formula(full_model)),
                       direction = "forward",trace = TRUE)

n <- nrow(data)
forward_bic <- stepAIC(null_model,scope = list(lower = formula(null_model),upper  = formula(full_model)),
                       direction = "forward",k = log(n),trace = TRUE)

vars_forward_aic <- attr(terms(forward_aic), "term.labels")
print(vars_forward_aic)

vars_forward_bic <- attr(terms(forward_bic), "term.labels")
print(vars_forward_bic)

save(vars_forward_aic, file = "Selection_variables/vars_forward_aic.RData")
save(vars_forward_bic, file = "Selection_variables/vars_forward_bic.RData")