# ===========================================================================
# SCRIPTS 4 - RECODIFICACIÓN Y VARIABLES ANALÍTICAS 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
# ===========================================================================
#

# 1- COPIO BASE ANALÍTICA -------------------------------------------------
# Luego de copiar, no modificamos la base analítica principal


# 2- CREACIÓN DE GRUPOS ETARIOS SEGÚN VIGILANCIA UCIRAG -------------------

# CREAR GRUPOS ETARIOS A PARTIR DE LA VARIABLE EDAD_UCIRAG 

#Grupos: menor de 6 meses, 6 a 23 meses, 2 a 14, 15-64 y 65 años y más


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


# 4- CREACIÓN DE CATEGORÍAS PARA VACUNACIÓN -------------------

# Aplicacion de vacuna VSR

aplicacion_vsr <- c("SE 32","SE 33", "SE 34","SE 35", "SE 36", "SE DESCONOCIDA")

# Categorías de vacunacion

vacunado <- c("CONSTATADA", "REFERIDA")

no_vacunado <- c("MADRE NO VACUNADA","NO VACUNADO")

# APLICO NUEVAS CATEGORÍAS A LAS VACUNAS ANTIGRIPAL MATERNA Y VSR 

data <- data %>% mutate (VAC_MATERNA_VSR = 
                           case_when(VAC_VSR %in% aplicacion_vsr ~ "VACUNADA",
                                     VAC_VSR %in% no_vacunado ~ "NO VACUNADA",
                                      TRUE ~ "SIN DATO"),
                                     
                         VAC_ANTIGRIPAL_MATERNA = 
                           case_when(VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                     VAC_ANTIGRIPAL_MATERNA %in% vacunado ~ "VACUNADA",
                                     TRUE ~ VAC_ANTIGRIPAL_MATERNA))

# APLICO NUEVAS CATEGORÍAS A LAS VACUNAS ANTIGRIPAL  

data <- data %>% mutate (VAC_ANTIGRIPAL = 
                           case_when(VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                     VAC_ANTIGRIPAL %in% vacunado ~ "VACUNADO",
                                      TRUE ~ "SIN DATO"))
