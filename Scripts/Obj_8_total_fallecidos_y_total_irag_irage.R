#======================================================================================
# GRAFICO TOTAL DE FALLECIDOS VS TOTAL POR IRAG E IRAG E
#======================================================================================


# Total de fallecidos por todas las causas y total de irag e irag e


curva_fallecidos_irag <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "percent",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = tabla_resumen$SEPI, #categorías en eje X
    title = list(text = "Año - Semana")) %>%  #título del eje X) 
  hc_yAxis(
    title = list(text = "Porcentaje de fallecidos"),
    labels = list(format = "{value}%"),
    max = 100) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = tabla_resumen$PROPORCION_FALLECIDOS_OTRAS_CAUSAS,
    name = "Otras causas",
    color = "lightgrey") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_FALLECIDOS,
    name = "Fallecidos por IRAG e IRAGe",
    color = "#252C61") %>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = "<span style='color:{point.color}'>●</span> {series.name}: <b>{point.percentage:.1f}%</b><br/>"
  ) 

curva_fallecidos_irag
