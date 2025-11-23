data = read.csv("data.csv", header = TRUE, sep = " ")

# Modèle avec les variables obligatoires
null_model <- glm(
  PNEUMONIA_YN ~ ARM_NUM + INCL_SEPSIS_YN + INCL_AKIN + AGE + SEX + BMI + SOFA,
  data = data,
  family = binomial
)

# Modèle complet
full_model <- glm(
  PNEUMONIA_YN ~ ., 
  data = data, 
  family = binomial
)

# Sélection Forward
forward_model <- step(
  null_model,
  scope = list(
    lower = formula(null_model),
    upper = formula(full_model)
  ),
  direction = "forward"
)

summary(forward_model)

vars_finales <- attr(terms(forward_model), "term.labels")
print(vars_finales)
