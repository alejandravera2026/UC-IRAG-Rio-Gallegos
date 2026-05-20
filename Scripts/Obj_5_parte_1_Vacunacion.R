#===============================================================================
# OBJETIVO 5
# CARACTERIZACIÓN DEL ANTECEDENTE DE VACUNACIÓN EN GRUPOS DE RIESGO
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS - HRRG
#===============================================================================


#===============================================================================
# 1. Selección de variables y construcción de grupos de riesgo
#===============================================================================

vacunacion_base <- data %>%
  select(
    EDAD_UC_IRAG,
    VAC_ANTIGRIPAL,
    VAC_ANTIGRIPAL_MATERNA,
    VAC_MATERNA_VSR
  ) %>%
  mutate(
    grupo_riesgo_vacunacion = case_when(
      EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses") ~ "Menores de 6 meses",
      EDAD_UC_IRAG %in% c("6 a 11 Meses", "12 a 23 Meses") ~ "6 a 23 meses",
      EDAD_UC_IRAG %in% c("65 a 69 Años", "70 a 74 Años", "75 y más Años") ~ 
        "65 años y más",
      TRUE ~ NA_character_
    ),
    grupo_riesgo_vacunacion = factor(
      grupo_riesgo_vacunacion,
      levels = c(
        "Menores de 6 meses",
        "6 a 23 meses",
        "65 años y más"
      )
    )
  ) %>%
  filter(!is.na(grupo_riesgo_vacunacion))


#===============================================================================
# 2. PARTE 1 - VACUNACIÓN MATERNA EN PACIENTES MENORES DE 6 MESES
#===============================================================================

vacunacion_materna_larga <- vacunacion_base %>%
  filter(grupo_riesgo_vacunacion == "Menores de 6 meses") %>%
  select(
    `Antigripal materna` = VAC_ANTIGRIPAL_MATERNA,
    `VSR materna` = VAC_MATERNA_VSR
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Vacuna",
    values_to = "Estado"
  ) %>%
  mutate(
    Estado = case_when(
      Estado %in% c("VACUNADA", "VACUNADO") ~ "Vacunada",
      Estado %in% c("NO VACUNADA", "NO VACUNADO") ~ "No vacunada",
      TRUE ~ "Sin dato"
    ),
    Estado = factor(
      Estado,
      levels = c("Vacunada", "No vacunada", "Sin dato")
    )
  )


#-------------------------------------------------------------------------------
# Tabla 1 - Vacunación antigripal materna
#-------------------------------------------------------------------------------

tabla_antigripal_materna <- vacunacion_materna_larga %>%
  filter(Vacuna == "Antigripal materna") %>%
  count(Estado, name = "casos") %>%
  mutate(
    porcentaje = round(casos / sum(casos) * 100, 1),
    resultado = paste0(casos, " (", porcentaje, "%)")
  ) %>%
  select(Estado, resultado) %>%
  gt() %>%
  cols_label(
    Estado = "Estado",
    resultado = "Resultado"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = "Vacunación antigripal materna.",
    subtitle = "Pacientes menores de 6 meses. Unidad Centinela HRRG, 2024–2026."
  )

tabla_antigripal_materna


#-------------------------------------------------------------------------------
# Tabla 2 - Vacunación materna contra VSR
#-------------------------------------------------------------------------------

tabla_vsr_materna <- vacunacion_materna_larga %>%
  filter(Vacuna == "VSR materna") %>%
  count(Estado, name = "casos") %>%
  mutate(
    porcentaje = round(casos / sum(casos) * 100, 1),
    resultado = paste0(casos, " (", porcentaje, "%)")
  ) %>%
  select(Estado, resultado) %>%
  gt() %>%
  cols_label(
    Estado = "Estado",
    resultado = "Resultado"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = "Vacunación materna contra VSR.",
    subtitle = "Pacientes menores de 6 meses. Unidad Centinela HRRG, 2024–2026."
  )

tabla_vsr_materna
