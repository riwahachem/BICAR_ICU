library(pROC)

data = read.csv2("data.csv",sep=" ")
data$PNEUMONIA_YN = factor(data$PNEUMONIA_YN)

# Sélection de variables
load("vars_forward.Rdata")
data_f <- data[, vars_forward]

set.seed(123)
d = sort(sample(nrow(data_f), nrow(data_f) * 0.8))
data_f_app = data_f[d, ]
data_f_test = data_f[-d, ]

modele = glm(PNEUMONIA_YN ~., data = data_f_app, family = binomial)

pred = predict(modele, newdata = data_f_test, type = "response")
pred_class <- factor(ifelse(pred > 0.3, "1", "0"))

conf <- caret::confusionMatrix(pred_class, data_f_test$PNEUMONIA_YN, positive = "1")

roc <- roc(data_f_test$PNEUMONIA_YN, pred, levels = c("0", "1"), direction = "<")

auc_value <- auc(roc_obj)

plot(roc, main = "Courbe ROC")
