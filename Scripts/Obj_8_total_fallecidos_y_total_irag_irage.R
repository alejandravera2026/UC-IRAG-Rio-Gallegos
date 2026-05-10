#======================================================================================
# GRAFICO TOTAL DE FALLECIDOS VS TOTAL POR IRAG E IRAG E
#======================================================================================

grafico_fallecidos <-highchart() %>%
  hc_chart(type = "column") %>%
  hc_title (text = "Proporción de fallecidos por IRAG/IRAGe vs otras causas") %>%
  hc_xAxis(categories = tabla_resumen$SEPI) %>%
  hc_yAxis(
    title = list(text = "Porcentaje"),
    max = 100,
    labels = list(format = "{value}%")) %>%
  hc_plotOptions(
    column = list(stacking = "normal", dataLabels = list(enabled = TRUE, format = "{y}%")))%>%
  hc_add_series(
    name = "Fallecidos IRAG/IRAGe",
    data = tabla_resumen$PROPORCION_FALLECIDOS,
    color = "#c62828") %>%
  hc_add_series(
    name ="Fallecidos otras causas",
    data = tabla_resumen$PROPORCION_FALLECIDOS_OTRAS_CAUSAS,
    color = "#90a4ae")

grafico_fallecidos
