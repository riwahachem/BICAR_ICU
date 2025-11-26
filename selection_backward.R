library(MASS) 
library(dplyr)

data <- read.csv("data.csv")  

vars_obligatoires <- c("ARM_NUM", "INCL_SEPSIS_YN","INCL_AKIN","AGE_CLASS","SEX","BMI","SOFA")

mod_full <- glm(PNEUMONIA_YN ~ .,
                data = data, family = binomial)

backward_aic <- stepAIC(mod_full, direction = "backward", trace = TRUE)

n <- nrow(data)
backward_bic <- stepAIC(mod_full, direction = "backward", k = log(n), trace = TRUE)

candidats_selec_aic <- attr(terms(backward_aic), "term.labels")
candidats_selec_bic <- attr(terms(backward_bic), "term.labels")

vars_backward_aic <- c(candidats_selec_aic, vars_obligatoires)
vars_backward_bic <- c(candidats_selec_bic, vars_obligatoires)

save(vars_backward_aic, file = "vars_backward_aic.RData")
save(vars_backward_bic, file = "vars_backward_bic.RData")
