library(dplyr)

admission = read.csv2("Data/ADMISSION.csv", sep =',')
admission = admission[,c("SUBJID", "ADMIN_SURGICAL", "Acidaemia_reason_admin")]
admission = admission %>% rename(ADMIN_REASON=ADMIN_SURGICAL)

baseline_clin = read.csv2("Data/BASELINE_CLIN.csv", sep =',')
baseline_clin = baseline_clin[,c(2, 5:18)]

biology = read.csv2("Data/BIOLOGY.csv", sep =',')
biology = biology[, c(2, 5:11)]

inclusion = read.csv2("Data/INCLUSION.csv", sep =',')
inclusion = inclusion[, c(2,5:7)]

nosocomial = read.csv2("Data/NOSOCOMIAL.csv", sep =',')
nosocomial = nosocomial[, c(2,5)]

randomization = read.csv2("Data/RANDOMIZATION.csv", sep =',')
randomization = randomization[, c(2, 5:7, 10)]

sofa = read.csv2("Data/SOFA.csv", sep =',')
sofa = sofa[, c(2, 5:10)]
SOFA = rowSums(sofa[,2:7])
sofa = data.frame(SUBJID = sofa[,"SUBJID"], SOFA)

table = list(admission, baseline_clin, biology, inclusion, nosocomial, randomization, sofa)
data = Reduce(function(x,y) full_join(x,y, by = "SUBJID"), table)

library(lubridate)

data <- data %>%
  mutate(
    AGE = year(ymd(CONSENT_DATE)) - BIRTH
  ) %>%
  dplyr::select(-BIRTH, -CONSENT_DATE)

data <- data %>%
  mutate(across(.cols = where(is.character), .fns = as.numeric))

write.csv(data, "data.csv", row.names = FALSE)
