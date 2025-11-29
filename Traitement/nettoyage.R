library(dplyr)
library(lubridate)

################################################################################

#Pré-requis : télécharger toutes les bases de données du CRF 

# Thème : prédiction des infections pulmonaires

# Ce script contient  : 
# 1) Le chargement des tables
# 2) L'extraction des variables utiles pour chaque table
# 3) La préparation des données 
# 4) La jointure des tables 
# 5) La création de la table finale

################################################################################

# Renommage de la variable ADMIN_SURGICAL en ADMIN_REASON :
# (0 = admission médicale, 1 = admission chirurgicale)

admission = read.csv2("ADMISSION.csv", sep =',')
admission = admission[,c(2,6,7)]
admission = admission %>% rename(ADMIN_REASON=ADMIN_SURGICAL)

baseline_clin = read.csv2("BASELINE_CLIN.csv", sep =',')
baseline_clin = baseline_clin[,c(2, 5:18)]

biology = read.csv2("BIOLOGY.csv", sep =',')
biology = biology[, c(2, 5:11)]

inclusion = read.csv2("INCLUSION.csv", sep =',')
inclusion = inclusion[, c(2,5:7)]

nosocomial = read.csv2("NOSOCOMIAL.csv", sep =',')
nosocomial = nosocomial[, c(2,5)]

randomization = read.csv2("RANDOMIZATION.csv", sep =',')
randomization = randomization[, c(2, 5:7, 10)]

# Calcul du score SOFA à l'admission (somme des composants)

sofa = read.csv2("SOFA.csv", sep =',')
sofa = sofa[, c(2, 5:10)]
SOFA = rowSums(sofa[,2:7])
sofa = data.frame(SUBJID = sofa[,"SUBJID"], SOFA)

# Jointure des tables sur la variable d'identification SUBJID

table = list(admission, baseline_clin, biology, inclusion, nosocomial, randomization, sofa)
data = Reduce(function(x,y) full_join(x,y, by = "SUBJID"), table)

# Création de la variable AGE (différence entre la date d'admission et la date de naissance)

data <- data %>%
  mutate(
    AGE = year(ymd(CONSENT_DATE)) - BIRTH
  ) %>%
  dplyr::select(-BIRTH, -CONSENT_DATE)

# Conversion des variables character en numérique pour la cohérence des formats

data <- data %>%
  mutate(across(.cols = where(is.character), .fns = as.numeric))

# Création de la table finale

write.csv(data, "data.csv", row.names = FALSE)
