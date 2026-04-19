# Determinar las comorbilidades por grupo etario

# En primer lugar se agrupa en una columna comorbilidades
data <- data %>% filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología")

colnames(data)

#filtro hasta la semana la semana 8 de 2026

ANIO_MINIMO <- 2024

SEMANA_MINIMA <- 18

ANIO_MAXIMO <- 2026

SEMANA_MAXIMA <- 8





data <- data %>%
  mutate(COMORBILIDAD = case_when(DIABETES == 1 ~ "Diabetes", HIPERTENSION == 1~ "Hipertensión",
                                  FUMADOR == 1 ~ "Fumador", ENF_RESPIRATORIA == 1 ~ "Enfermedad respiratoria",
                                  ENF_CEREBROVASCULAR == 1 ~ "Enfermedad cerebrovascular", ENF_NEUROLOGICA_CRONICA == 1 ~ "Enfermedad neurológica crónica",
                                  ENF_HEPATICA == 1 ~ "Enfermedad hepática", ENF_NEUROMUSCULAR == 1 ~ "Enfermedad neuromuscular", 
                                  ENF_CARDIACA == 1 ~ "Enfermedad cardíaca", ENF_REUMATOLOGICA == 1 ~ 
                                    "Enfermedad reumatológica",ENF_RENAL == 1 ~ "Enfermeda renal", SIN_COMORBILIDADES == 1 ~ "sin comorbilidades",
                                  INMUNOCOMPROMETIDO_OTRAS_CAUSAS == 1 ~ "Inmunosupresión por otras causas", VIH == 1 ~ "Infección por VIH/SIDA",
                                  CANCER == 1 ~ "Enfermedad oncológica", TRASPLANTADO == 1 ~ "Transplantado", BAJO_PESO_NACIMIENTO == 1 ~"Bajo peso de nacimiento",
                                  TUBERCULOSIS == 1~ "Tuberculosis", CARDIOPATIA_CONGENITA == 1 ~ "Cardiopatía congénita",
                                  ASPLENIA == 1~ "Asplenia", BRONQUIOLITIS_PREVIA == 1~ "Bronquiolitis previa" ,
                                  EMBARAZO_COMORBILIDAD ==1 ~ "Embarazo comorbilidad", DISCAPACIDAD_INTELECTUAL == 1 ~ "Discapacidad intelectual",
                                  ASPIRINA ==1 ~ "Aspirina", OBESIDAD ==1 ~ "Obesidad", PREMATURIDAD == 1 ~ "Prematuridad",
                                  PREMATURIDAD_MEN33SG == 1 ~"Prematuridad < 33 semanas", PREMATURIDAD_33A36SG == 1 ~ "Prematuridad 33 a 36 semanas",
                                  ASMA == 1~ "Asma", DESNUTRICION == 1~ "Desnutrición", EMBARAZO_PUERPERIO == 1 ~ "Embarazo Puerperio",
                                  RN_TERMINO == 1 ~ "Recién nacido a término", S_DOWN == 1~ "Síndrome de Down", OTRAS_COMORBILIDADES ==1 ~ "Otras comorbilidades",
                                  PRESENCIA_COMORBILIDADES ==1 ~"Presencia de comorbililidades"))
#Se realiza una tabla en general de las comorbilidades 2024, 2025 y 2026

data4 <- data  %>%
  count(COMORBILIDAD) %>%
  mutate(porcentaje= n/sum (n)) %>%
  gt() %>%
  fmt_percent(columns = porcentaje, decimals = 1)

data4


# Menor de 2 años




#Entre 2 a 60 años


# Mayor de 60 años