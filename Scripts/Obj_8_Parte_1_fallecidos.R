#============================================================================
# BASE PARA GRÁFICO DE DEFUNCIONES - BARRAS AGRUPADAS
#============================================================================

fallecidos_grafico <- tabla_resumen %>%
  
  select(
    SEPI,
    `Defunciones por IRAG`,
    `Defunciones por IRAG extendida`
  ) %>%
  
  mutate(
    `Defunciones por IRAG` = ifelse(
      is.na(`Defunciones por IRAG`),
      0,
      `Defunciones por IRAG`
    ),
    
    `Defunciones por IRAG extendida` = ifelse(
      is.na(`Defunciones por IRAG extendida`),
      0,
      `Defunciones por IRAG extendida`
    )
  )
#============================================================================
# GRÁFICO DE BARRAS AGRUPADAS - DEFUNCIONES POR IRAG E IRAG EXTENDIDA
#============================================================================

grafico_fallecidos_agrupado <- highchart() %>%
  
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Defunciones semanales IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = "Recuento absoluto. Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = fallecidos_grafico$SEPI,
    title = list(text = "Semana epidemiológica y año"),
    labels = list(
      rotation = -45,
      step = 6
    ),
    lineColor = "#BFBFBF",
    tickColor = "#BFBFBF"
  ) %>%
  
  hc_yAxis(
    min = 0,
    title = list(text = "Número de defunciones"),
    allowDecimals = FALSE,
    gridLineColor = "#E6E6E6"
  ) %>%
  
  hc_plotOptions(
    column = list(
      stacking = NULL,
      borderWidth = 0,
      pointPadding = 0.02,
      groupPadding = 0.04,
      pointWidth = 8,
      dataLabels = list(
        enabled = FALSE
      )
    )
  ) %>%
  
  hc_add_series(
    name = "IRAG",
    data = fallecidos_grafico$`Defunciones por IRAG`,
    color = "#252C61"
  ) %>%
  
  hc_add_series(
    name = "IRAG extendida",
    data = fallecidos_grafico$`Defunciones por IRAG extendida`,
    color = "#4DB6E2"
  ) %>%
  
  hc_tooltip(
    shared = TRUE,
    useHTML = TRUE,
    headerFormat = "<b>{point.key}</b><br/>",
    pointFormat = paste0(
      "<span style='color:{series.color}'>●</span> ",
      "{series.name}: <b>{point.y}</b> defunciones<br/>"
    )
  ) %>%
  
  hc_legend(
    align = "center",
    verticalAlign = "bottom"
  ) %>%
  
  hc_credits(
    enabled = FALSE
  )

grafico_fallecidos_agrupado
