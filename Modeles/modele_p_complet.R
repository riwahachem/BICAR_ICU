library(pROC)
library(caret)

data <- read.csv("data.csv")  
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

vars <- c(
  "Acidaemia_reason_admin",
  "INCL_LAB_serum_creatinine",
  "SEX",
  "SUBJID",
  "SOFA",
  "ATCD_Severe_liver_insufficiency",
  "INCL_LAB_PaCO2",
  "INCL_LAB_serum_bicar",
  "ATCD_Cirrhosis",
  "ATCD_Smoking",
  "ATCD_Immunocompromised",
  "ADMIN_REASON",
  "INCL_LAB_PaO2toFiO2_ratio",
  "BMI",
  "ATCD_Alcohol_abuse",
  "AGE",
  "ATCD_Chronic_hypertension",
  "ATCD_Cheart_failure",
  "INCL_LAB_blood_urea_nitrogen",
  "INCL_LAB_arterial_pH",
  "ATCD_Ckidney_disease",
  "ATCD_Crespiratory_insufficiency",
  "INCL_IMV"
)
data_b <- data[, !(names(data) %in% vars)]

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

best = coords(roc, "best", best.method="youden", ret = c("threshold","sensitivity","specificity"))
best