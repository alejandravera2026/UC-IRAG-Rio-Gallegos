#============================================================================
# SCRIPT 6 - OBJETIVO 3 - PORCENTAJE DE POSITIVIDAD
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- SELECCIÓN DE VARIABLES -----------------------------------------------

positividad_base <- data %>% 
  select(SEPI, VSR_FINAL, INFLUENZA_FINAL, COVID_19_FINAL)


# 2- CONVERSIÓN A FORMATO LARGO -------------------------------------------

positividad_larga <- positividad_base %>%
  
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


# 3- CÁLCULO DE POSITIVIDAD SEMANAL ---------------------------------------

positividad_semanal <- positividad_larga %>%
  
  group_by(SEPI, Agente) %>%
  
  summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    POSITIVIDAD = round(POSITIVOS / ESTUDIADOS * 100, 1),
    .groups = "drop"
  ) %>%
  
  filter(ESTUDIADOS >= 5)


# 4- CONVERSIÓN A FORMATO ANCHO PARA GRÁFICO ------------------------------

positividad_grafico <- positividad_semanal %>%
  
  select(SEPI, Agente, POSITIVIDAD) %>%
  
  pivot_wider(
    names_from = Agente,
    values_from = POSITIVIDAD,
    values_fill = 0
  )


# 5- GRÁFICO INTERACTIVO TIPO SCATTER -------------------------------------

positividad_scatter <- highchart() %>%
  
  hc_chart(type = "scatter") %>%
  
  hc_title(
    text = "Positividad semanal por virus respiratorio"
  ) %>%
  
  hc_subtitle(
    text = "Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = positividad_grafico$SEPI,
    title = list(text = NULL),
    
    labels = list(
      rotation = -45,
      step = 4
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "Poisitividad %"),
    min = 0,
    max = 100,
    tickInterval = 10
  ) %>%
  
  hc_plotOptions(
    scatter = list(
      lineWidth = 2,
      marker = list(
        radius = 4,
        symbol = "circle"
      )
    )
  ) %>%
  
  hc_add_series(
    name = "Influenza",
    data = positividad_grafico$Influenza,
    color = "#E69F00"
  ) %>%
  
  hc_add_series(
    name = "VSR",
    data = positividad_grafico$VSR,
    color = "#009E73"
  ) %>%
  
  hc_add_series(
    name = "SARS-CoV-2",
    data = positividad_grafico$`SARS-CoV-2`,
    color = "#A61C3C"
  )

positividad_scatter
