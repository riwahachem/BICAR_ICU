library(pROC)
library(caret)

data <- read.csv("data.csv")  
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("Selection_variables/vars_backward_aic.Rdata")
data_b <- data[, c(vars_backward_aic,"PNEUMONIA_YN")]

set.seed(123)
d = sort(sample(nrow(data_b), nrow(data_b) * 0.8))
data_b_app = data_b[d, ]
data_b_test = data_b[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_b_app, family = binomial)

pred = predict(modele, newdata = data_b_test, type = "response")

roc <- roc(data_b_test$PNEUMONIA_YN, pred)

auc_value <- auc(roc)

plot(roc,
     legacy.axes = TRUE, 
     col="red",
     lwd         = 3, 
     xlab = "1 - Spécificité (Taux de faux positifs)",
     ylab = "Sensibilité (Taux de vrais positifs)",
     main = paste0("Courbe ROC - Modèle backward (AUC = ", round(auc_value, 3), ")")
)
