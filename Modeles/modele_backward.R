library(pROC)
library(caret)

################################################################################

# Pré-requis : 
# Exécution du fichier Traitement/nettoyage.R pour générer data.csv 
# Exécution du fichier Selection_variables/selection_backward.R pour générer vars_backward_aic.Rdata 

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

load("Selection_variables/vars_backward_aic.Rdata")
data_b <- data[, c(vars_backward_aic,"PNEUMONIA_YN")]
data_b = subset(data_b,select=-ATCD_Ckidney_disease)

set.seed(123)
d = sort(sample(nrow(data_b), nrow(data_b) * 0.8))
data_b_app = data_b[d, ]
data_b_test = data_b[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_b_app, family = binomial)
summary(modele)

pred = predict(modele, newdata = data_b_test, type = "response")
plot(density(pred), main = "Densité des probabilités prédites", xlab = "Probabilité prédite", ylab = "Densité", lwd = 2)

roc <- roc(data_b_test$PNEUMONIA_YN, pred)

auc_value <- auc(roc)

plot(roc,
     legacy.axes = TRUE, 
     col="red",
     lwd         = 3, 
     xlab = "False positive rate",
     ylab = "True positive rate"
)
legend("bottomright", legend = paste("AUC =", round(auc_value, 3))) 

best = coords(roc, "best", best.method="youden", ret = c("threshold","sensitivity","specificity"))
best