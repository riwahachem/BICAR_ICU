library(pROC)
library(caret)
packages <- c("dplyr", "mice", "missForest", "VIM")
installed <- packages %in% rownames(installed.packages())
if (any(!installed)) {
  install.packages(packages[!installed], dependencies = TRUE)
}

lapply(packages, library, character.only = TRUE)

data <- read.csv("data.csv")
data$PNEUMONIA_YN <- factor(data$PNEUMONIA_YN)

set.seed(123)
d = sort(sample(nrow(data), nrow(data) * 0.8))
data_app  = data[d, ]
data_test = data[-d, ]

## Train :
fumeurs_app = subset(data_app,  ADMIN_REASON == 1)
nonfumeurs_app <- subset(data_app,  ADMIN_REASON == 0)

## Test :
fumeurs_test   <- subset(data_test, ADMIN_REASON == 1)

nonfumeurs_test <- subset(data_test, ADMIN_REASON == 0)


load("Selection_variables/vars_backward_aic.Rdata")
fumeurs_app <- fumeurs_app[, c(vars_backward_aic,"PNEUMONIA_YN")]
fumeurs_app <- fumeurs_app[, !(names(fumeurs_app) %in% "ADMIN_REASON")]

nonfumeurs_app = nonfumeurs_app[, c(vars_backward_aic,"PNEUMONIA_YN")]
nonfumeurs_app <- nonfumeurs_app[, !(names(nonfumeurs_app) %in% "ADMIN_REASON")]

fumeurs_test = fumeurs_test[, c(vars_backward_aic,"PNEUMONIA_YN")]
fumeurs_test <- fumeurs_test[, !(names(fumeurs_test) %in% "ADMIN_REASON")]

nonfumeurs_test = nonfumeurs_test[, c(vars_backward_aic,"PNEUMONIA_YN")]
nonfumeurs_test <- nonfumeurs_test[, !(names(nonfumeurs_test) %in% "ADMIN_REASON")]

modele_fumeurs = glm(PNEUMONIA_YN ~., data = fumeurs_app, family = binomial)
modele_nonfumeurs = glm(PNEUMONIA_YN ~., data = nonfumeurs_app, family = binomial)

pred_fumeurs = predict(modele_fumeurs, newdata = fumeurs_test, type = "response")
pred_nonfumeurs = predict(modele_nonfumeurs, newdata = nonfumeurs_test, type = "response")

pred_class_fumeurs    <- factor(ifelse(pred_fumeurs > 0.2, "1", "0"))
pred_class_nonfumeurs <- factor(ifelse(pred_nonfumeurs > 0.2, "1", "0"))

pred_finales = c(pred_class_fumeurs,pred_class_nonfumeurs)
  
pred_finales = as.numeric(pred_finales)

vraies_valeurs = c(fumeurs_test$PNEUMONIA_YN, nonfumeurs_test$PNEUMONIA_YN)

roc = roc(vraies_valeurs, pred_finales)

auc_global <- auc(roc)

plot(roc,
     legacy.axes = TRUE,
     col = "red",
     lwd = 3,
     xlab = "1 - Spécificité (Faux positifs)",
     ylab = "Sensibilité (Vrais positifs)",
     main = paste0("Courbe ROC - Prédiction finale (AUC = ",
                   round(auc_global, 3), ")")
)