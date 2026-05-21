#============================================================================
# SCRIPT 6 - OBJETIVO 3 - RESULTADOS DE VIRUS POR SEMANA EPIDEMIOLÓGICA
# GRÁFICO DE BARRAS APILADAS
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


#============================================================================
# 1- SELECCIÓN DE VARIABLES
#============================================================================

virus_base <- data %>% 
  select(
    SEPI,
    VSR_FINAL,
    INFLUENZA_FINAL,
    COVID_19_FINAL
  )


#============================================================================
# 2- CONVERSIÓN A FORMATO LARGO
#============================================================================

virus_largo <- virus_base %>%
  
  pivot_longer(
    cols = c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
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
      Agente == "VSR_FINAL" ~ "VSR",
      Agente == "COVID_19_FINAL" ~ "SARS-CoV-2",
      TRUE ~ NA_character_
    ),
    
    CATEGORIA = case_when(
      resultado == "Negativo" ~ "Negativos",
      resultado != "Negativo" & Agente == "Influenza" ~ "Influenza",
      resultado != "Negativo" & Agente == "VSR" ~ "VSR",
      resultado != "Negativo" & Agente == "SARS-CoV-2" ~ "SARS-CoV-2",
      TRUE ~ NA_character_
    )
  )
#============================================================================
# 3- RECUENTO DE DETERMINACIONES POR SEMANA Y CATEGORÍA
#============================================================================

virus_semanal <- virus_largo %>%
  
  group_by(SEPI, CATEGORIA) %>%
  
  summarise(
    N = n(),
    .groups = "drop"
  )

#============================================================================
# 4- FORMATO ANCHO PARA EL GRÁFICO
#============================================================================

virus_semanal_grafico <- virus_semanal %>%
  
  pivot_wider(
    names_from = CATEGORIA,
    values_from = N,
    values_fill = 0
  )
#============================================================================
# 5- ASEGURAR QUE EXISTAN TODAS LAS COLUMNAS
#============================================================================

if(!"Influenza" %in% names(virus_semanal_grafico)
   ) virus_semanal_grafico$Influenza <- 0
if(!"VSR" %in% names(virus_semanal_grafico)
   ) virus_semanal_grafico$VSR <- 0
if(!"SARS-CoV-2" %in% names(virus_semanal_grafico)
   ) virus_semanal_grafico$`SARS-CoV-2` <- 0
if(!"Negativos" %in% names(virus_semanal_grafico)
   ) virus_semanal_grafico$Negativos <- 0

#============================================================================
# 6- GRÁFICO DE BARRAS APILADAS POR SEMANA EPIDEMIOLÓGICA
#============================================================================

grafico_virus_semanal_apilado <- highchart() %>%
  
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Determinaciones para virus respiratorios por 
    semana epidemiológica y año"
  ) %>%
  
  hc_subtitle(
    text = "Resultados positivos para Influenza, VSR y SARS-CoV-2, 
    y determinaciones negativas. Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = virus_semanal_grafico$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 4
    )
  ) %>%
  
  hc_yAxis(
    min = 0,
    title = list(text = "Número de determinaciones"),
    reversedStacks = FALSE,
    gridLineColor = "#E6E6E6"
  ) %>%
  
  hc_plotOptions(
    column = list(
      stacking = "normal",
      borderWidth = 0,
      pointPadding = 0.05,
      groupPadding = 0.05,
      dataLabels = list(
        enabled = FALSE
      )
    )
  ) %>%
  
  hc_add_series(
    name = "Influenza",
    data = virus_semanal_grafico$Influenza,
    color = "#E69F00"
  ) %>%
  
  hc_add_series(
    name = "VSR",
    data = virus_semanal_grafico$VSR,
    color = "#009E73"
  ) %>%
  
  hc_add_series(
    name = "SARS-CoV-2",
    data = virus_semanal_grafico$`SARS-CoV-2`,
    color = "#A61C3C"
  ) %>%
  
  hc_add_series(
    name = "Negativos",
    data = virus_semanal_grafico$Negativos,
    color = "#D9D9D9"
  ) %>%
  
  hc_tooltip(
    shared = TRUE
  )

grafico_virus_semanal_apilado
