#=========================================================================
# Objetivo 7: 
#Grafico interactivo proporcion de ingresos a UCI por IRAG e IRAGe
# Estimar la proporción de casos que requieren ingreso a 
#unidades de cuidados intensivos, en relación con el total 
#de pacientes que ingresaron a UTI, por semana epidemiológica y grupo etario, 
#como indicador de severidad. 
 
#=========================================================================


curva_uci_irag <- highchart() %>%
  
  hc_chart(type = "line") %>%
  
  hc_title(
    text = "Ingreso a Terapia Intensiva por IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = "Proporción semanal sobre el total de ingreso a terapia intensiva. 
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
    title = list(text = "% de UCI"),
    labels = list(format = "{value}%"),
    min = 0,
    max = 100,
    tickInterval = 2
  ) %>%
  
  hc_add_series(
    name = "Terapia intensiva por IRAG",
    data = tabla_resumen$IRAG_UCI,
    color = "#252C61",
    lineWidth = 2.5,
    marker = list(
      enabled = TRUE,
      radius = 3
    )
  ) %>%
  hc_add_series(
    name = "Terapia intensiva por IRAG extendida",
    data = tabla_resumen$IRAGE_UCI,
    color = "#4DB6E2",
    lineWidth = 2.5,
    marker = list(
      enabled = TRUE,
      radius = 3
      
    )) %>%
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


curva_uci_irag
