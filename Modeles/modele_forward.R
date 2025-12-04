library(pROC)
library(caret)

################################################################################

# Pré-requis : 
# Exécution du fichier Traitement/nettoyage.R pour générer data.csv 
# Exécution du fichier Selection_variables/selection_forward.R pour générer vars_forward_aic.Rdata 

# Ce script contient  : 
# 1) Le chargement de la table data
# 2) Le chargement des variables à garder après sélection
# 3) L'échantillonnage de la table 
# 4) L'ajustement du modèle logistique complet
# 5) Le calcul des probabilités d'avoir une infection pulmonaire pour chaque individu
# 6) L'affichage de la courbe ROC
# 7) L'identification du seuil optimal par la méthode de Youden

################################################################################

data <- read.csv("Data/data.csv")  
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("Selection_variables/vars_forward_aic.Rdata")
data_f <- data[, c(vars_forward_bic,"PNEUMONIA_YN")]

set.seed(123)
d = sort(sample(nrow(data_f), nrow(data_f) * 0.8))
data_f_app = data_f[d, ]
data_f_test = data_f[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_f_app, family = binomial)

pred = predict(modele, newdata = data_f_test, type = "response")

roc <- roc(data_f_test$PNEUMONIA_YN, pred)

auc_value <- auc(roc)

plot(roc,
     legacy.axes = TRUE, 
     col="blue",
     lwd         = 3, 
     xlab = "Taux de faux positifs",
     ylab = "Taux de vrais positifs",
     main        = paste0("Courbe ROC du modèle après sélection forward")
)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3))) 

best = coords(roc, "best", best.method="youden", ret = c("threshold","sensitivity","specificity"))
best