library(pROC)

data = read.csv2("data.csv",sep=" ")
data <- data.frame(lapply(data, function(x) {
  if (is.character(x)) as.numeric(x) else x
}))
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("vars_backward_aic.Rdata")
data_b <- data[, c(vars_backward_aic,"PNEUMONIA_YN")]

set.seed(123)
d = sort(sample(nrow(data_b), nrow(data_b) * 0.8))
data_b_app = data_b[d, ]
data_b_test = data_b[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_b_app, family = binomial)

pred = predict(modele, newdata = data_b_test, type = "response")
pred_class <- factor(ifelse(pred > 0.3, "1", "0"))

conf <- caret::confusionMatrix(pred_class, data_b_test$PNEUMONIA_YN, positive = "1")

roc <- roc(data_b_test$PNEUMONIA_YN, pred, levels = c("0", "1"), direction = "<")

auc_value <- auc(roc)

plot(roc,
     legacy.axes = TRUE, 
     col="red",
     lwd         = 3, 
     main        = paste0("Courbe ROC - Modèle backward ",
                          AUC = round(auc_value, 3))
)
