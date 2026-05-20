curva_fallecidos_irag <- highchart() %>%
  
  hc_chart(type = "line") %>%
  
  hc_title(
    text = "Fallecidos por IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = "Proporción semanal sobre el total de fallecidos. 
    Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = tabla_resumen$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 6
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "% de fallecidos"),
    labels = list(format = "{value}%"),
    min = 0,
    max = 50,
    tickInterval = 2
  ) %>%
  
  hc_add_series(
    name = "Fallecidos",
    data = tabla_resumen$PROPORCION_FALLECIDOS,
    color = "#252C61",
    lineWidth = 2.5,
    marker = list(
      enabled = TRUE,
      radius = 3
    )
  ) %>%
  
  hc_tooltip(
    shared = TRUE,
    pointFormat = paste0(
      "<span style='color:{point.color}'>●</span> ",
      "{series.name}: <b>{point.y:.1f}%</b><br/>"
    )
  ) %>%
  
  hc_legend(
    align = "center",
    verticalAlign = "bottom"
  )

curva_fallecidos_irag
