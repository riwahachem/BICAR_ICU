library(ggplot2)
library(dplyr)

data = read.csv2("data.csv",sep=",")
Y = data$PNEUMONIA_YN
Y = as.factor(Y)

# Analyse variable cible
table(Y)
prop.table(table(Y))

ggplot(data, aes(x = factor(SEX), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon le sexe",
    x = "Sexe",
    y = "Effectif",
    fill = "Pneumonie"
  )

ggplot(data, aes(x = factor(ADMIN_REASON), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon la raison de la venue",
    x = "Raison médicale ou chirurgicale",
    y = "Nombre",
    fill = "Pneumonie"
  )

ggplot(data, aes(x = factor(ATCD_Smoking), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon s'il est fumeur",
    x = "Fumeur ou non",
    y = "Nombre",
    fill = "Pneumonie"
  )

data$INCL_IMV <- as.factor(data$INCL_IMV)

ggplot(data, aes(x = factor(INCL_IMV), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon s'il a eu une IMV",
    x = "IMV ou non",
    y = "Nombre",
    fill = "Pneumonie"
  )

ggplot(data, aes(x = factor(ARM_NUM), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon son groupe",
    x = "Groupe",
    y = "Nombre",
    fill = "Pneumonie"
  )

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = SOFA, fill = factor(PNEUMONIA_YN))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Distribution du score SOFA selon la pneumonie",
    x = "Pneumonie",
    y = "SOFA"
  )

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = AGE, fill = factor(PNEUMONIA_YN))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Distribution de l'âge selon la pneumonie",
    x = "Pneumonie",
    y = "AGE"
  )

data$BMI <- as.numeric(data$BMI)

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = BMI, fill = factor(PNEUMONIA_YN))) +
  geom_boxplot() +
  labs(
    title = "Distribution du BMI selon la pneumonie",
    x = "Pneumonie",
    y = "BMI"
  ) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred"))



