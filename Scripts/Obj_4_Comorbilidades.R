#============================================================================
# SCRIPT 6 - OBJETIVO 4
# CARACTERIZACIÓN DE COMORBILIDADES SEGÚN GRUPO ETARIO Y CLASIFICACIÓN
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- REDODIFICACIÓN DE COMORBILIDAD Y ORDEN DE GRUPOS ETARIOS -------------

comorbilidad_base <- data %>%
  mutate(
    COMORBILIDAD = case_when(
      PRESENCIA_COMORBILIDADES == 1 ~ "Sí",
      PRESENCIA_COMORBILIDADES == 2 ~ "No",
      PRESENCIA_COMORBILIDADES == 9 ~ "Sin datos",
      TRUE ~ "Sin datos"
    ),
    grupo_etario = factor(
      grupo_etario,
      levels = c(
        "< 2 años",
        "2-14 años",
        "15-59 años",
        "60 años y más"
      )
    )
  ) %>%
  select(
    grupo_etario,
    COMORBILIDAD
  )


# 2- VERIFICACIÓN RÁPIDA --------------------------------------------------

table(comorbilidad_base$grupo_etario, useNA = "always")
table(comorbilidad_base$COMORBILIDAD, useNA = "always")


# 3- TABLA DE RESUMEN -----------------------------------------------------

tabla_comorbilidad_grupo <- comorbilidad_base %>%
  
  filter(
    !is.na(grupo_etario),
    COMORBILIDAD %in% c("Sí", "No", "Sin datos")
  ) %>%
  
  count(grupo_etario, COMORBILIDAD, name = "casos") %>%
  
  group_by(grupo_etario) %>%
  
  mutate(
    porcentaje = round(casos / sum(casos) * 100, 1),
    
    resultado = paste0(
      casos,
      " (",
      sprintf("%.1f", porcentaje),
      "%)"
    )
  ) %>%
  
  ungroup() %>%
  
  select(
    grupo_etario,
    COMORBILIDAD,
    resultado
  ) %>%
  
  pivot_wider(
    names_from = COMORBILIDAD,
    values_from = resultado,
    values_fill = "0 (0.0%)"
  ) %>%
  
  select(
    grupo_etario,
    `Sí`,
    `No`,
    `Sin datos`
  )


# 4- TABLA FINAL EN FORMATO gt --------------------------------------------

tabla_comorbilidad_grupo_gt <- tabla_comorbilidad_grupo %>%
  
  gt() %>%
  
  cols_label(
    grupo_etario = "Grupo etario",
    `Sí` = "Con comorbilidades",
    `No` = "Sin comorbilidades",
    `Sin datos` = "Sin datos"
  ) %>%
  
  cols_align(
    align = "center"
  ) %>%
  
  tab_header(
    title = "Comorbilidades según grupo etario",
    subtitle = "Unidad Centinela HRRG, 2024–2026"
  )

tabla_comorbilidad_grupo_gt
