library(MASS) 

data <- read.csv("data.csv")  

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
data <- data[, !(names(data) %in% vars)]
# Modèle complet
modele <- glm(PNEUMONIA_YN ~ ., data = data, family = binomial)
summary(modele)

s <- summary(modele)$coefficients
s_no_intercept <- s[-1, , drop = FALSE]
var_max_p <- rownames(s_no_intercept)[which.max(s_no_intercept[, "Pr(>|z|)"])]
var_max_p