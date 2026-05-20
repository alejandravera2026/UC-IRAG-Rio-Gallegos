#============================================================================
# OBJETIVO 10
# AGENTES ETIOLÓGICOS POSITIVOS EN CASOS FALLECIDOS
#============================================================================

#============================================================================
# 1. Selección de variables
#============================================================================

agente_etiologico_causa <- data %>%
  select(
    grupo_etario,
    INFLUENZA_FINAL,
    VSR_FINAL,
    COVID_19_FINAL,
    FALLECIDO
  )


#============================================================================
# 2. Paso a formato largo
#============================================================================

agente_etiologico_causa <- agente_etiologico_causa %>%
  pivot_longer(
    cols = c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
    names_to = "Agente",
    values_to = "resultado"
  ) %>%
  filter(
    !is.na(resultado),
    resultado != "Sin resultado"
  ) %>%
  mutate(
    Agente = case_when(
      Agente == "INFLUENZA_FINAL" ~ "Influenza",
      Agente == "COVID_19_FINAL" ~ "SARS-CoV-2",
      Agente == "VSR_FINAL" ~ "VSR"
    )
  )


#============================================================================
# 3. Tabla de positividad en fallecidos
#============================================================================

tabla_fallecidos_agente <- agente_etiologico_causa %>%
  filter(FALLECIDO == "SI") %>%
  filter(!is.na(grupo_etario)) %>%
  mutate(
    grupo_etario = case_when(
      grupo_etario == "15-59 años" ~ "15 a 59 años",
      grupo_etario == "60 años y más" ~ "60 años y más",
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
  group_by(grupo_etario, Agente) %>%
  summarise(
    estudiados = n(),
    positivos = sum(resultado != "Negativo"),
    positividad = round(positivos / estudiados * 100, 1),
    .groups = "drop"
  ) %>%
  filter(positivos > 0) %>%
  mutate(
    resultado = paste0(
      positivos, "/", estudiados, " (", positividad, "%)"
    )
  ) %>%
  arrange(grupo_etario, Agente)


#============================================================================
# 4. Tabla final GT
#============================================================================

tabla_fallecidos_agente_gt <- tabla_fallecidos_agente %>%
  select(
    grupo_etario,
    Agente,
    resultado
  ) %>%
  pivot_wider(
    names_from = Agente,
    values_from = resultado,
    values_fill = "-"
  ) %>%
  select(
    grupo_etario,
    Influenza,
    `SARS-CoV-2`,
    VSR
  ) %>%
  gt() %>%
  cols_label(
    grupo_etario = "Grupo etario",
    Influenza = "Influenza",
    `SARS-CoV-2` = "SARS-CoV-2",
    VSR = "VSR"
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  tab_header(
    title = "Agentes etiológicos positivos en casos fallecidos",
    subtitle = "Frecuencia de detección y positividad por agente respiratorio. Unidad Centinela HRRG, 2024–2026"
  )

tabla_fallecidos_agente_gt
