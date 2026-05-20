# ===========================================================================
# SCRIPTS 4 - RECODIFICACIÓN - GRUPOS ETARIOS Y NUEVAS CATEGORÍAS 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
# ===========================================================================

# 1- CREACIÓN DE GRUPOS ETARIOS SEGÚN LA VARIABLE EDAD_UCIRAG  ------------
     # Grupos: < 6 meses; 6 a 23 meses; 2 a 14; 15-64 y 65 años y más.

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


# 2- VERIFICAMOS DATOS FALTANTES POR VARIABLES ----------------------------

colSums(is.na(data))

  # Observamos la distribución de grupos etarios y sus porcentajes

prop.table(table(data$grupo_etario))*100


# 3- CREACIÓN DE CATEGORÍAS PARA VACUNACIÓN  ------------------------------

aplicacion_vsr <- c("SE 32", "SE 33", "SE 34", "SE 35","SE 36", 
                    "SE DESCONOCIDA")


# 4- CATEGORÍAS DE VACUNACIÓN ---------------------------------------------

vacunado <- c("CONSTATADA", "REFERIDA")

no_vacunado <- c("MADRE NO VACUNADA","NO VACUNADO")


# 5- APLICO NUEVAS CATEGORÍAS A LAS VACUNAS ANTIGRIPAL MATERNA ------------

data <- data %>%
  
  mutate (VAC_MATERNA_VSR = case_when
          (VAC_VSR %in% aplicacion_vsr ~ "VACUNADA",
            VAC_VSR %in% no_vacunado ~ "NO VACUNADA",
            TRUE ~ "SIN DATO")
          )%>%
  
  mutate (VAC_ANTIGRIPAL_MATERNA = case_when
          (VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
            VAC_ANTIGRIPAL_MATERNA %in% vacunado ~ "VACUNADA",
            TRUE ~ "SIN DATO")
          )


# 6- APLICO NUEVAS CATEGORÍAS A LAS VACUNAS ANTIGRIPAL  -------------------

data <- data %>% 
  
  mutate (VAC_ANTIGRIPAL = case_when
          (VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
            VAC_ANTIGRIPAL %in% vacunado ~ "VACUNADO",
            TRUE ~ "SIN DATO")
          )
