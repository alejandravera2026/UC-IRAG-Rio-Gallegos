
#==============================================================================
# INGRESOS A UCI POR IRAG E IRAG EXTENDIDA
# Porcentaje + valores absolutos en tooltip
#==============================================================================

tabla_resumen <- tabla_resumen %>%
  mutate(
    PROPORCION_IRAG_UCI = round(
      (`Casos de IRAG entre los ingresados a UCI` / `Pacientes ingresados a UCI`) * 100, 
      1
    ),
    
    PROPORCION_IRAGE_UCI = round(
      (`Casos de IRAG extendida entre los ingresados a UCI` / `Pacientes ingresados a UCI`) * 100, 
      1
    )
  )


curva_uci_irag <- highchart() %>%
  
  hc_chart(type = "line") %>%
  
  hc_title(
    text = "Ingresos a UCI por IRAG e IRAG extendida"
  ) %>%
  
  hc_subtitle(
    text = paste(
      "Proporción semanal sobre el total de ingresos a UCI.",
      "Unidad Centinela HRRG, 2024–2026"
    )
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
    title = list(text = "% de ingresos a UCI"),
    labels = list(format = "{value}%"),
    min = 0,
    max = 100,
    tickInterval = 10
  ) %>%
  
  hc_add_series(
    name = "IRAG",
    data = purrr::pmap(
      list(
        y = tabla_resumen$PROPORCION_IRAG_UCI,
        n = tabla_resumen$`Casos de IRAG entre los ingresados a UCI`,
        d = tabla_resumen$`Pacientes ingresados a UCI`
      ),
      function(y, n, d) {
        list(y = y, n = n, d = d)
      }
    ),
    color = "#252C61",
    lineWidth = 2.5,
    marker = list(
      enabled = TRUE,
      radius = 3
    )
  ) %>%
  
  hc_add_series(
    name = "IRAG extendida",
    data = purrr::pmap(
      list(
        y = tabla_resumen$PROPORCION_IRAGE_UCI,
        n = tabla_resumen$`Casos de IRAG extendida entre los ingresados a UCI`,
        d = tabla_resumen$`Pacientes ingresados a UCI`
      ),
      function(y, n, d) {
        list(y = y, n = n, d = d)
      }
    ),
    color = "#4DB6E2",
    lineWidth = 2.5,
    marker = list(
      enabled = TRUE,
      radius = 3
    )
  ) %>%
  
  hc_tooltip(
    shared = TRUE,
    useHTML = TRUE,
    formatter = JS(
      "
      function() {
        let s = '<b>' + this.x + '</b><br/>';

        this.points.forEach(function(point) {
          s += '<span style=\"color:' + point.color + '\">●</span> ' +
               point.series.name + ': <b>' +
               point.y.toFixed(1) + '%</b> (' +
               point.point.n + '/' + point.point.d + ')<br/>';
        });

        return s;
      }
      "
    )
  ) %>%
  
  hc_legend(
    align = "center",
    verticalAlign = "bottom"
  )

curva_uci_irag
