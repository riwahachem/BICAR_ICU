library(MASS) 

data <- read.csv("data.csv")  

# Modèle avec les variables obligatoires
null_model <- glm(
  PNEUMONIA_YN ~ ARM_NUM + INCL_SEPSIS_YN + INCL_AKIN + AGE_CLASS,
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
forward_aic <- stepAIC(
  null_model,
  scope = list(
    lower = formula(null_model),
    upper  = formula(full_model)
  ),
  direction = "forward",
  trace = TRUE
)

n <- nrow(data)
forward_bic <- stepAIC(
  null_model,
  scope = list(
    lower = formula(null_model),
    upper  = formula(full_model)
  ),
  direction = "forward",
  k = log(n),
  trace = TRUE
)

vars_forward_aic <- attr(terms(forward_aic), "term.labels")
print(vars_forward_aic)

vars_forward_bic <- attr(terms(forward_bic), "term.labels")
print(vars_forward_bic)

save(vars_forward_aic, file = "Selection_variables/vars_forward_aic.RData")
save(vars_forward_bic, file = "Selection_variables/vars_forward_bic.RData")