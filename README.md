# BICAR-ICU

---
L'étude se porte sur l’efficacité du traitement au bicarbonate de sodium sur des 
patients en soins intensifs avec une acidose métabolique sévère.  
Nous nous sommes intéresser en particulier à la survenue d’une infection nosocomiale, 
durant les 28 jours de suivis. 
---

## Structure du projet

```text
BICAR_ICU/  
├── Data/  
│   ├── ADMISSION.csv  
│   ├── BASELINE_CLIN.csv  
│   ├── BIOLOGY.csv  
│   ├── INCLUSION.csv  
│   ├── NOSOCOMIAL.csv  
│   ├── RANDOMIZATION.csv  
│   ├── SOFA.csv  
│   └── data.csv  
├── Traitement/  
│   ├── analyse_descriptive.R  
│   ├── nettoyage.R  
├── Selection_variables/  
│   ├── selection_backward.R
│   ├── selection_forward.R  
│   └── selection_p_value.R
├── Modeles/  
│   ├── modele_backward.R  
│   ├── modele_forward.R  
│   └── modele_p_value.R  
├── .gitignore  
└── README.md  
```

---

## Auteurs

- **Marine GERMAIN** – [marine.germain@etu.umontpellier.fr](mailto:marine.germain@etu.umontpellier.fr)  
- **Riwa HACHEM REDA** - [riwa.hachem-reda@etu.umontpellier.fr](mailto:riwa.hachem-reda@etu.umontpellier.fr)  
- **Coralie ROMANI DE VINCI** – [coralie.romani-de-vinci@etu.umontpellier.fr](coralie.romani-de-vinci@etu.umontpellier.fr)

Ce projet a été réalisé dans le cadre de notre Master 2 Statistiques pour l'Information et l'Aide à la Décision, sous l’encadrement d'Elodie Brunel-Piccinini et Maïlis Amico.

---
