library(pROC)
library(caret)

data <- read.csv("data.csv")  
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("Selection_variables/vars_forward_bic.Rdata")
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
     xlab = "1 - Spécificité (Taux de faux positifs)",
     ylab = "Sensibilité (Taux de vrais positifs)",
     main        = paste0("Courbe ROC - Modèle forward (AUC = ",round(auc_value, 3),")")
)