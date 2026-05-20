#============================================================================
# OBJETIVO 11
# TASA DE LETALIDAD POR GRUPO ETARIO
#============================================================================

#============================================================================
# 1. Selección de variables
#============================================================================

letalidad_irag <- data %>%
  select(
    grupo_etario,
    FALLECIDO,
    CLASIFICACION_MANUAL
  )


#============================================================================
# 2. Casos IRAG e IRAGe
#============================================================================

letalidad_irag <- letalidad_irag %>%
  
  filter(
    CLASIFICACION_MANUAL %in% c(
      "Infección respiratoria aguda grave (IRAG)",
      "IRAG extendida"
    )
  ) %>%
  
  filter(!is.na(grupo_etario))


#============================================================================
# 3. Tabla de letalidad
#============================================================================

tabla_letalidad <- letalidad_irag %>%
  
  group_by(grupo_etario) %>%
  
  summarise(
    casos = n(),
    
    fallecidos = sum(FALLECIDO == "SI", na.rm = TRUE),
    
    letalidad = round(fallecidos / casos * 100, 1),
    
    .groups = "drop"
  ) %>%
  
  mutate(
    resultado = paste0(
      fallecidos,
      "/",
      casos,
      " (",
      letalidad,
      "%)"
    ),
    
    grupo_etario = case_when(
      grupo_etario == "15-59 años" ~ "15 a 59 años",
      TRUE ~ grupo_etario
    ),
    
    grupo_etario = factor(
      grupo_etario,
      levels = c(
        "< 2 años",
        "2-14 años",
        "15 a 59 años",
        "60 años y más"
      )
    )
  ) %>%
  
  arrange(grupo_etario)


#============================================================================
# 4. Tabla final
#============================================================================

tabla_letalidad_gt <- tabla_letalidad %>%
  
  select(
    grupo_etario,
    casos,
    fallecidos,
    resultado
  ) %>%
  
  gt() %>%
  
  cols_label(
    grupo_etario = "Grupo etario",
    casos = "Casos",
    fallecidos = "Fallecidos",
    resultado = "Letalidad"
  ) %>%
  
  cols_align(
    align = "center"
  ) %>%
  
  tab_header(
    title = "Tasa de letalidad por grupo etario",
    
    subtitle = paste(
      "Fallecidos/casos y porcentaje de letalidad.",
      "Unidad Centinela HRRG, 2024–2026"
    )
  )

tabla_letalidad_gt
