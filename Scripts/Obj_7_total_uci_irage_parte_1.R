#==============================================================================
# INGRESOS A UCI POR IRAG E IRAG EXTENDIDA
# Porcentaje semanal + numerador/denominador en tooltip
#==============================================================================

tabla_resumen <- tabla_resumen %>%
  mutate(
    PROPORCION_IRAG_UCI = round(
      (`Casos de IRAG entre los ingresados a UCI` /
         `Pacientes ingresados a UCI`) * 100,
      1
    ),
    
    PROPORCION_IRAGE_UCI = round(
      (`Casos de IRAG extendida entre los ingresados a UCI` /
         `Pacientes ingresados a UCI`) * 100,
      1
    )
  )


curva_uci_irag <- highchart() %>%
  
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Ingresos a UCI por IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = "Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_xAxis(
    categories = tabla_resumen$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "N° de ingresos a UCI"), allowDecimals = FALSE) %>%
  hc_plotOptions(column = list(stacking = "normal", borderWidht = 0)) %>%
  hc_colors(c ("#4f4f4f","#4db6e2", "#252c61")) %>%
  hc_add_series(name = "Total Ingresos a UCI", data =
                  tabla_resumen$`Pacientes ingresados a UCI`) %>%
  hc_add_series(name = "Casos de IRAG a UCI", data =
                  tabla_resumen$`Casos de IRAG entre los ingresados a UCI`) %>%
  hc_add_series(name = "Casos de IRAG extendida a UCI", data = 
                  tabla_resumen$`Casos de IRAG extendida entre los ingresados a UCI`)%>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = "<span style='color:{point.color}'>●</span> 
    {series.name}: <b>{point.y}</b> casos<br>")%>%
  hc_legend(layout = "horizontal",
                  align = "center",
                  verticalAlign = "bottom",
            reversed = FALSE
                  )%>%
  hc_credits(enabled = FALSE)
  
                
              
curva_uci_irag
  