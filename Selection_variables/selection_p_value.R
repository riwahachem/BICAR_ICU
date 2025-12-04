library(MASS) 

################################################################################

#Pré-requis : Exécution du fichier Traitement/nettoyage.R pour générer data.csv 

# Ce script contient  : 
# 1) Le chargement de la table data
# 2) L'extraction des variables de stratification à conserver
# 3) La sélection backward selon les p-values
# 4) L'extraction et la sauvegarde des variables retenues par la sélection

################################################################################

data <- read.csv("Data/data.csv")  

vars_obligatoires <- c("ARM_NUM", "INCL_SEPSIS_YN","INCL_AKIN","AGE_CLASS")

# Définition du seuil de p-value
alpha <- 0.05

vars_cand <- setdiff(names(data), c("PNEUMONIA_YN", vars_obligatoires))

vars_retirees <- character(0)

# Boucle qui retire la variable ayant la plus grande p-value 
# jusqu'à ce que toutes les variables soient significatives. 

repeat {
  vars_modele <- c(vars_obligatoires, vars_cand)
  
  formule <- as.formula(paste("PNEUMONIA_YN ~", paste(vars_modele, collapse = " + ")))
  modele  <- glm(formule, data = data, family = binomial)
  
  s <- summary(modele)$coefficients
  s_no_intercept <- s[-1, , drop = FALSE]
  
  idx_cand <- rownames(s_no_intercept) %in% vars_cand
  p_cand   <- s_no_intercept[idx_cand, "Pr(>|z|)"]
  
  if (length(p_cand) == 0) break
  
  var_max <- names(which.max(p_cand))
  p_max   <- max(p_cand)
  
  if (p_max < alpha) break
  
  vars_cand    <- setdiff(vars_cand, var_max)
  vars_retirees <- c(vars_retirees, var_max)
  vars_backward_pvalue <- setdiff(names(data), c("PNEUMONIA_YN", vars_retirees))
}

save(vars_backward_pvalue, file = "Selection_variables/vars_backward_pvalue.RData")