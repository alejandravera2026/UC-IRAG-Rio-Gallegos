#==============================================================================
# GRAFICO DE TOTAL DE INTERNADOS POR TODAS LAS CAUSAS Y TOTAL DE INTERNADOS POR 
#IRAG E IRAG E POR SEMANA Y POR AÑO
#==============================================================================


curva_internaciones_irag <- highchart() %>%
  
  hc_chart(type = "spline") %>%
  
  hc_title(
    text = "Internaciones por IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = "Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = tabla_resumen$SEPI,
    title = list(text = NULL),
    labels = list(rotation = -45, step = 6)
  ) %>%
  
  hc_yAxis(
    title = list(text = "% de internaciones"),
    labels = list(format = "{value}%"),
    min = 0,
    max = 10,
    tickInterval = 2,
    gridLineColor = "#E6E6E6"
  ) %>%
  
  hc_add_series(
    name = "IRAG",
    data = purrr::pmap(
      list(
        y = tabla_resumen$PROPORCION_IRAG,
        n = tabla_resumen$`Casos de IRAG entre los internados`,
        d = tabla_resumen$`Pacientes internados por todas las causas`
      ),
      function(y, n, d) list(y = y, n = n, d = d)
    ),
    color = "#252C61",
    lineWidth = 2.5,
    marker = list(enabled = TRUE, radius = 3)
  ) %>%
  
  hc_add_series(
    name = "IRAG extendida",
    data = purrr::pmap(
      list(
        y = tabla_resumen$PROPORCION_IRAGE,
        n = tabla_resumen$`Casos de IRAG extendida entre los internados`,
        d = tabla_resumen$`Pacientes internados por todas las causas`
      ),
      function(y, n, d) list(y = y, n = n, d = d)
    ),
    color = "#4DB6E2",
    lineWidth = 2.5,
    marker = list(enabled = TRUE, radius = 3)
  ) %>%
  
  hc_tooltip(
    shared = TRUE,
    useHTML = TRUE,
    pointFormat = paste0(
      "<span style='color:{point.color}'>●</span> ",
      "{series.name}: <b>{point.y:.1f}%</b> ",
      "({point.n}/{point.d})<br/>"
    )
  ) %>%
  
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    itemStyle = list(fontWeight = "normal", fontSize = "11px")
  )

curva_internaciones_irag
