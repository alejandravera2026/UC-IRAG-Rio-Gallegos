
#================================================================================
# 📊 Grafico interactivo proporcion de casos de IRAG e IRAGe entre los internados   
#================================================================================


#===============================================================================  
# Realizo la curva internados por todas las causas e internados por IRAG e IRAGe
# de acuerdo a las proporciones calculadas
#================================================================================

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


#======================================================================================
# 📊 Grafico interactivo proporcion de fallecidos por IRAG e IRAGe entre los fallecidos   
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


#=========================================================================
# 📊 Grafico interactivo proporcion de ingresos a UCI por IRAG e IRAGe    
#=========================================================================

# Total de ingresados a UCI por todas las causas y total de irag e irag e

curva_uci_irag_irage <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "percent",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = tabla_resumen$SEPI, #categorías en eje X
    title = list(text = "Año - Semana")) %>%  #título del eje X) 
  hc_yAxis(
    title = list(text = "Porcentaje de ingresos a UCI"),
    labels = list(format = "{value}%"),
    max = 100) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = tabla_resumen$PROPORCION_UCI_OTRAS_CAUSAS,
    name = "Otras causas",
    color = "lightgrey") %>%
  hc_add_series(
    data = tabla_resumen$IRAG_UCI,
    name = "Internados en UCI por IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = tabla_resumen$IRAGE_UCI,
    name = "Internados en UCI por IRAGe",
    color = "#7EC8E6") %>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = "<span style='color:{point.color}'>●</span> {series.name}: <b>{point.percentage:.1f}%</b><br/>"
  ) 

curva_uci_irag_irage

