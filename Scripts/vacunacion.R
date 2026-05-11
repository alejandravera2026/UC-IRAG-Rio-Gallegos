# Selecciono las columnas para estudiar los vacunados dentro de los grupos de edad

vacunacion <- data %>%
  select(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION,VAC_ANTIGRIPAL, VAC_ANTIGRIPAL_MATERNA,
         VAC_VSR, EDAD_UC_IRAG)

# Armo vectores de grupos de edad
unique(vacunacion$EDAD_UC_IRAG)

menores_6_meses <- c("0 a 2 Meses", "3 a 5 Meses")

Entre_7_a_23_meses <- c("6 a 11 Meses", "12 a 23 Meses")

mayores_60_años <- c("60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años")

#Determino dentro de los grupos de edad 
# En menores de 6 meses entran las mamás si fueron o no vacunadas

unique(vacunacion$VAC_ANTIGRIPAL_MATERNA)

unique(vacunacion$VAC_VSR)

# Deberia ver sin dato o realmente eliminar del plan de estudio


unique(vacunacion_materna$VAC_ANTIGRIPAL_MATERNA)

vacunacion_materna <- vacunacion %>%
  filter(VAC_ANTIGRIPAL_MATERNA != "SIN DATO"& VAC_VSR != "SIN DATO")%>% 
  filter(EDAD_UC_IRAG %in% menores_6_meses)%>%
  select(EDAD_UC_IRAG,VAC_ANTIGRIPAL_MATERNA,VAC_VSR,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION)


# Frecuencia relativa de madre vacunada influenza

Vacuna_materna_influenza_constatada <- sum(vacunacion_materna$VAC_ANTIGRIPAL_MATERNA
                                             == "CONSTATADA", ra.rm = TRUE)
vacuna_materna_influenza_referida <- sum(vacunacion_materna$VAC_ANTIGRIPAL_MATERNA
                                         == "REFERIDA", ra.rm = TRUE)
no_vacuna_materna_influenza <- sum(vacunacion_materna$VAC_ANTIGRIPAL_MATERNA
                                   == "MADRE NO VACUNADA", ra.rm = TRUE)

total_vacuna <- sum(Vacuna_materna_influenza_constatada + vacuna_materna_influenza_referida)
total_vacuna_no_vacuna <- sum(total_vacuna + no_vacuna_materna_influenza)

# Frecuencia relativa influenza materna

proporcion_vacuna_materna_influenza <- round((total_vacuna/total_vacuna_no_vacuna)*100.1)
  
# Frecuencia absoluta Vacuna VSR

unique(vacunacion_materna$VAC_VSR)

vacuna_materna_vsr_36 <- sum(vacunacion_materna$VAC_VSR == "SE 36", ra.rm = TRUE)
vacuna_materna_vsr_35 <- sum(vacunacion_materna$VAC_VSR == "SE 35", ra.rm = TRUE)
vacuna_materna_vsr_32 <- sum(vacunacion_materna$VAC_VSR == "SE 32", ra.rm= TRUE)
vacuna_materna_vsr_desconocida <- sum(vacunacion_materna == "SE DESCONOCIDA", ra.rm= TRUE)
no_vacuna_vsr <- sum(vacunacion_materna$VAC_VSR == "MADRE NO VACUNADA", ra.rm = TRUE)

vacuna_materna_vsr <- sum(vacuna_materna_vsr_32 + vacuna_materna_vsr_35 + vacuna_materna_vsr_36 +
                            vacuna_materna_vsr_desconocida)
total_vacuna_vsr <- sum(vacuna_materna_vsr + no_vacuna_vsr)

proporcion_vaCUNA_VSR <- round((vacuna_materna_vsr/total_vacuna_vsr)*100.1)
