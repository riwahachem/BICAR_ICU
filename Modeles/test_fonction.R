library(dplyr)
library(MASS)
library(pROC)
library(caret)
library(mice)
library(missForest)
library(VIM)

data <- read.csv("data.csv")  

# Fonction d'analyse par sous-groupe
subgroup_model <- function(data, split_var, vars_selected, seed = 123) {
  data$PNEUMONIA_YN <- factor(data$PNEUMONIA_YN, levels = c(0, 1))
  
  # Split 80/20 global
  set.seed(seed)
  d <- sort(sample(nrow(data), nrow(data) * 0.8))
  data_app  <- data[d, ]
  data_test <- data[-d, ]
  
  # Sous-groupes train / test
  group1_app  <- subset(data_app,  data_app[[split_var]]  == 1)
  group0_app  <- subset(data_app,  data_app[[split_var]]  == 0)
  
  group1_test <- subset(data_test, data_test[[split_var]] == 1)
  group0_test <- subset(data_test, data_test[[split_var]] == 0)
  
  # Garder seulement les variables sélectionnées + Y
  group1_app  <- group1_app[,  c(vars_selected, "PNEUMONIA_YN")]
  group0_app  <- group0_app[,  c(vars_selected, "PNEUMONIA_YN")]
  group1_test <- group1_test[, c(vars_selected, "PNEUMONIA_YN")]
  group0_test <- group0_test[, c(vars_selected, "PNEUMONIA_YN")]
  
  # Retirer la variable de split des modèles
  group1_app  <- group1_app[, !(names(group1_app)  %in% split_var)]
  group0_app  <- group0_app[, !(names(group0_app)  %in% split_var)]
  group1_test <- group1_test[, !(names(group1_test) %in% split_var)]
  group0_test <- group0_test[, !(names(group0_test) %in% split_var)]
  
  # Modèles séparés
  modele_g1 <- glm(PNEUMONIA_YN ~ ., data = group1_app, family = binomial)
  modele_g0 <- glm(PNEUMONIA_YN ~ ., data = group0_app, family = binomial)
  
  # Prédictions séparées
  pred_g1 <- predict(modele_g1, newdata = group1_test, type="response")
  pred_g0 <- predict(modele_g0, newdata = group0_test, type="response")
  
  # Regroupement des prédictions
  pred_finales <- c(pred_g1, pred_g0)
  pred_finales <- as.numeric(pred_finales)
  
  # Regroupement des vraies valeurs
  vraies_valeurs <- c(group1_test$PNEUMONIA_YN, group0_test$PNEUMONIA_YN)
  
  # ROC + AUC
  roc_obj <- pROC::roc(vraies_valeurs, pred_finales)
  auc_global <- pROC::auc(roc_obj)
  
  # Plot ROC
  plot(
    roc_obj,
    legacy.axes = TRUE,
    col = "red",
    lwd = 3,
    xlab = "1 - Spécificité (Faux positifs)",
    ylab = "Sensibilité (Vrais positifs)",
    main = paste0("Courbe ROC - AUC = ", round(auc_global, 3))
  )
  
  # Retourne tout
  return(list(
    modele_g1 = modele_g1,
    modele_g0 = modele_g0,
    pred_g1   = pred_g1,
    pred_g0   = pred_g0,
    auc       = auc_global,
    roc       = roc_obj
  ))
}

load("Selection_variables/vars_backward_aic.Rdata")

res_fumeur = subgroup_model(data, "ATCD_Smoking", vars_backward_aic )
res_IMV = subgroup_model(data, "INCL_IMV", vars_backward_aic)
res_SEPSIS = subgroup_model(data, "INCL_SEPSIS_YN", vars_backward_aic)
res_SEX = subgroup_model(data, "SEX", vars_backward_aic)
res_AKIN = subgroup_model(data, "INCL_AKIN", vars_backward_aic)
res_AGE = subgroup_model(data, "AGE_CLASS", vars_backward_aic)
res_ARM = subgroup_model(data, "ARM_NUM", vars_backward_aic)
res_ADMIN_REASON = subgroup_model(data, "ADMIN_REASON", vars_backward_aic)
