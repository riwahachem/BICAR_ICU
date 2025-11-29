library(ggplot2)
library(dplyr)

################################################################################

#Pré-requis : Exécution du fichier Traitement/nettoyage.R pour générer data.csv 

# Ce script contient  : 
# 1) Le chargement de la table data
# 2) La description de la variable cible PNEUMONIA_YN
# 3) La visualisation du lien entre les variables qualitative et la variable cible (barblot)
# 4) La conversion des variables si besoin
# 5) La visualisation du lien entre les variables quantitatives et la variable cible (boxblot)

################################################################################


data = read.csv2("data.csv",sep=",")

Y = data$PNEUMONIA_YN
Y = as.factor(Y)
table(Y)
prop.table(table(Y))

#Visualisation de la variable SEX vs la variable cible

ggplot(data, aes(x = factor(SEX), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon le sexe",
    x = "Sexe",
    y = "Proportion",
    fill = "Pneumonie"
  )

#Visualisation de la variable ADMIN_REASON vs la variable cible

ggplot(data, aes(x = factor(ADMIN_REASON), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon la raison de l'admission",
    x = "Raison médicale ou chirurgicale",
    y = "Proportion",
    fill = "Pneumonie"
  )

#Visualisation de la variable ATCD_Smoking vs la variable cible

ggplot(data, aes(x = factor(ATCD_Smoking), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon s'il est fumeur",
    x = "Fumeur ou non",
    y = "Proportion",
    fill = "Pneumonie"
  )

data$INCL_IMV <- as.factor(data$INCL_IMV)

#Visualisation de la variable INCL_IMV vs la variable cible

ggplot(data, aes(x = factor(INCL_IMV), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon s'il a eu une IMV",
    x = "IMV ou non",
    y = "Proportion",
    fill = "Pneumonie"
  )

#Visualisation de la variable ARM_NUM vs la variable cible

ggplot(data, aes(x = factor(ARM_NUM), fill = factor(PNEUMONIA_YN))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Nombre de patients avec et sans pneumonie selon son groupe",
    x = "Groupe",
    y = "Proportion",
    fill = "Pneumonie"
  ) 

#Visualisation de la variable SOFA vs la variable cible

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = SOFA, fill = factor(PNEUMONIA_YN))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Distribution du score SOFA selon la pneumonie",
    x = "Pneumonie",
    y = "SOFA"
  ) +
  theme(legend.position = "none")

#Visualisation de la variable AGE vs la variable cible

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = AGE, fill = factor(PNEUMONIA_YN))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  labs(
    title = "Distribution de l'âge selon la pneumonie",
    x = "Pneumonie",
    y = "AGE"
  ) +
  theme(legend.position = "none")

data$BMI <- as.numeric(data$BMI)

#Visualisation de la variable BMI vs la variable cible

ggplot(data, aes(x = factor(PNEUMONIA_YN), y = BMI, fill = factor(PNEUMONIA_YN)
                 ))+
  geom_boxplot() +
  labs(
    title = "Distribution du BMI selon la pneumonie",
    x = "Pneumonie",
    y = "BMI"
  ) +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkred")) +
  theme(legend.position = "none")
