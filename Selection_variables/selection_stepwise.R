library(MASS) 

data = read.csv("data.csv", header = TRUE, sep = ",")

# Modèle avec les variables obligatoires
null_model <- glm(
  PNEUMONIA_YN ~ ARM_NUM + INCL_SEPSIS_YN + INCL_AKIN + AGE_CLASS + SEX + BMI + SOFA,
  data = data,
  family = binomial
)

# Modèle complet
full_model <- glm(
  PNEUMONIA_YN ~ ., 
  data = data, 
  family = binomial
)

# Stepwise 
stepwise_model <- stepAIC(
  null_model,
  scope = list(
    lower = formula(null_model),
    upper = formula(full_model)
  ),
  direction = "both"
)

vars_stepwise <- attr(terms(stepwise_model), "term.labels")
vars_stepwise
