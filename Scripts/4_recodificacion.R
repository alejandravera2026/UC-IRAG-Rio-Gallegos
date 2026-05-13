# ===========================================================================
# SCRIPTS 4 - RECODIFICACIÓN Y VARIABLES ANALÍTICAS 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
# ===========================================================================
#

# 1- COPIO BASE ANALÍTICA -------------------------------------------------
# Luego de copiar, no modificamos la base analítica principal


# 2- CREACIÓN DE GRUPOS ETARIOS SEGÚN VIGILANCIA UCIRAG -------------------

data <- data %>%
  mutate(
    grupo_etario = case_when(
      EDAD_DIAGNOSTICO < 2 ~ "< 2 años",
      EDAD_DIAGNOSTICO >= 2 &
        EDAD_DIAGNOSTICO <= 14 ~ "2-14 años",
      EDAD_DIAGNOSTICO >= 15 &
      EDAD_DIAGNOSTICO <= 59 ~ "15-59 años",
      EDAD_DIAGNOSTICO >= 60 ~ "60 años y más",
      TRUE ~ NA_character_
    )
  )

unique(data$grupo_etario)

# 3- CONTROLES BÁSICOS DE BASE ANALÍTICA ----------------------------------

  # Analizamos dimensiones de la base analítica

dim(data)

  # Verificamos datos faltantes por variable

colSums(is.na(data))

  # Observamos la distribución de grupos etarios

table(data$grupo_etario, useNA = "ifany")

  # Calculamos porcentajes por grupo etario

prop.table(table(data$grupo_etario))*100









