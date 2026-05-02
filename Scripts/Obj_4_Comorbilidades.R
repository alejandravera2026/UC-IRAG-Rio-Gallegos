#=========================================================================
# Objetivo: 4 Caracterizar las comorbilidades por IRAG e IRAGe grupo etario
#=========================================================================


# Se crea una columna  de comorbilidad

unique(data$COMORBILIDAD)

tabla_comorbilidad_grupo <- data %>%
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiologia") %>%
  mutate(COMORBILIDAD = case_when(
    PRESENCIA_COMORBILIDADES == 1  ~ "SI", 
    PRESENCIA_COMORBILIDADES == 2  ~ "NO",
    TRUE ~ "SIN DATO"
  ))
  

# Se define la tabla de comorbilidad por grupo de edad

unique(tabla_comorbilidad_grupo$EDAD_DIAGNOSTICO)


# Se crea tabla de comorbilidad por grupo de edad establecidos
           
tabla_comorbilidad_menor_dos <- tabla_comorbilidad_grupo %>%
  filter(COMORBILIDAD %in% c("SI", "NO")
           mutate
  mutate(grupo_etario_resumen = case_when(
    EDAD_DIAGNOSTICO))
    
  
  count(EDAD_UC_IRAG, COMORBILIDAD) %>%
  tidyr::pivot_wider(
    names_from = COMORBILIDAD,
    values_from = n,
    values_fill = 0) %>%
  mutate(total = SI + NO,
         Con_comorbilidad = SI/total,
         Sin_comorbilidad = NO/total) %>%
  gt() %>%
  fmt_percent(
    columns = c(Con_comorbilidad, Sin_comorbilidad),
    decimals = 1)

tabla_comorbilidad_menor_dos


