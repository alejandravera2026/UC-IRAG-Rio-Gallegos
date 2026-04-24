
#===============================================================
# ⚠️ Calculo porporciones sobre la base agrupada
#===============================================================

# Creo variable fallecidos por IRAG e IRAGe 

tabla_resumen <- tabla_resumen %>% mutate(FALLECIDOS_IRAG = (`Defunciones por IRAG` + `Defunciones por IRAG extendida`))

# Proporcion IRAG e IRAGe sobre ingresos totales

colnames(tabla_resumen)

tabla_resumen <- tabla_resumen %>% mutate(PROPORCION_IRAG = round(
  (`Casos de IRAG entre los internados`/`Pacientes internados por todas las causas`)*100,1),
  PROPORCION_IRAGE = round(
    (`Casos de IRAG extendida entre los internados`/`Pacientes internados por todas las causas`)*100,1),
  PROPORCION_INTERNADOS_OTRAS_CAUSAS = (100-(PROPORCION_IRAG + PROPORCION_IRAGE)),
  PROPORCION_FALLECIDOS = round(
    (FALLECIDOS_IRAG/`Defunciones totales`)*100,1))

tabla_resumen <- tabla_resumen %>% 
  mutate(PROPORCION_FALLECIDOS_OTRAS_CAUSAS = 100 - PROPORCION_FALLECIDOS)

#================================================================================
# 📊 Grafico interactivo proporcion de casos de IRAG e IRAGe entre los internados   
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
    name = "IRAG",
    color = "#7fc97f") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_IRAGE,
    name = "IRAGe",
    color = "#beaed4")

curva_internaciones_irag

# Total de fallecidos por todas las causas y total de irag e irag e
# ESA TABLA SOLO MUESTRA LAS PROPORCIONES, PERO NO MUESTRA EL TOTAL DE FALLECIDOS QUE NO HAY CASOS DE IRAG E IRAG E


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
    name = "FALLECIDOS POR IRAG E IRAG EXTENDIDA",
    color = "#7fc97f")

curva_fallecidos_irag
