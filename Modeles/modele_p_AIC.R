library(pROC)
library(caret)

data <- read.csv("data.csv")  
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("Selection_variables/vars_forward_aic.Rdata")
data_f <- data[, c(vars_forward_aic,"PNEUMONIA_YN")]

vars = c("SEX","SOFA","BMI","ATCD_Ckidney_disease","ATCD_Crespiratory_insufficiency","INCL_IMV")
data_f <- data_f[, !(names(data_f) %in% vars)]

set.seed(123)
d = sort(sample(nrow(data_f), nrow(data_f) * 0.8))
data_f_app = data_f[d, ]
data_f_test = data_f[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_f_app, family = binomial)
summary(modele)

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

best = coords(roc, "best", best.method="youden", ret = c("threshold","sensitivity","specificity"))
best