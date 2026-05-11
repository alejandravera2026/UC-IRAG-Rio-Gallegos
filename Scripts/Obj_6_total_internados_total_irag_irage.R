#==============================================================================
# GRAFICO DE TOTAL DE INTERNADOS POR TODAS LAS CAUSAS Y TOTAL DE INTERNADOS POR 
#IRAG E IRAG E POR SEMANA Y POR AÑO
#==============================================================================


#===============================================================================  
# Realizo la curva internados por todas las causas e internados por IRAG e IRAGe
# de acuerdo a las proporciones calculadas
#================================================================================
##
curva_internaciones_irag <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "percent",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = tabla_resumen$SEPI, #categorías en eje X
    title = list(text = "Año - Semana")) %>%  #título del eje X) 
  hc_yAxis(
    title = list(text = "Porcentaje de ingresos"),
    labels = list(format = "{value}%"),
    max = 100) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = tabla_resumen$PROPORCION_INTERNADOS_OTRAS_CAUSAS,
    name = "Otras causas",
    color = "lightgrey") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_IRAG,
    name = "Internados por IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_IRAGE,
    name = "Internados por IRAGe",
    color = "#7EC8E6"
  )%>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = paste0(
      "<span style='color:{point.color}'>●</span> ",
      "{series.name}: <b>{point.percentage:.1f}%</b><br/>"
    )
  )

curva_internaciones_irag
