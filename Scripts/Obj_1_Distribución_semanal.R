#============================================================================
# SCRIPT 5 - OBJETIVO 1 - DISTRIBUCIÓN TEMPORAL 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- SE CREA UN NUEVO OBJETO ----------------------------------------------

data_obj1 <- base_final

 # Agrupo casos por año y semana epidemiológica

casos_semana_anio <- data_obj1 %>% 
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 

 # Paso datos a formato ancho (wider) para hacer curva interactiva

casos_semana_anio <- casos_semana_anio %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# 2- CURVA INTERACTIVA CASOS DE IRAG E IRAGE -------------------------------

curva_interactiva <-highchart() %>%
  
  hc_chart(
    type = "column",
    spacingTop = 20
  ) %>%
  
  hc_title(
    text = "Casos de IRAG e IRAG extendida")%>%
  
  hc_subtitle(
    text = "Unidad Centinela HRRG, 2024 - 2026."
  )%>%
  
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_semana_anio$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 6
    )) %>%
  
  hc_yAxis(title= list(text="Número de casos"),
           gridLineColor = "#E6E6E6"
           ) %>%
  
  hc_add_series(
    data = casos_semana_anio$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  
  hc_add_series(
    data = casos_semana_anio$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6") %>%
  
  hc_tooltip(
    shared = TRUE,
    pointFormat = paste0(
      "<span style='color:{point.color}'>●</span> ",
      "{series.name}: <b>{point.y}</b><br/>"
    )
  ) %>%
  
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    
    itemStyle = list(
      fontWeight = "normal",
      fontSize = "11px"
    )
  )
  
curva_interactiva


# 3- TOTALES Y PORCENTAJES ------------------------------------------------

base_final %>%
  count(CLASIFICACION_MANUAL)

notificaciones_totales <- base_final %>%
  
  filter(CLASIFICACION_MANUAL %in%
           c("Infección respiratoria aguda grave (IRAG)",
             "IRAG extendida")
         )%>%
  summarise(Total = n())

notificaciones_totales

notificaciones_irag <- base_final %>%
  
  filter(CLASIFICACION_MANUAL %in%
           "Infección respiratoria aguda grave (IRAG)") %>%
  summarise(IRAG = n()
            )

notificaciones_irag

notificaciones_irage <- base_final %>%
  
  filter(CLASIFICACION_MANUAL %in% "IRAG extendida") %>%
  summarise(IRAGE = n()
            )

notificaciones_irage

pct_IRAG <- base_final %>%
  
  filter(CLASIFICACION_MANUAL %in% 
           c("Infección respiratoria aguda grave (IRAG)", 
             "IRAG extendida")
         ) %>%
  
  summarise(
    n_IRAG = sum(CLASIFICACION_MANUAL == 
                   "Infección respiratoria aguda grave (IRAG)"),
    n_Total = n())%>%
  
  mutate(pct = round(n_IRAG/n_Total *100.1))%>%
  
  pull(pct)

pct_IRAG

pct_IRAGE <- base_final %>%
  
  filter(CLASIFICACION_MANUAL %in% 
           c("Infección respiratoria aguda grave (IRAG)",
             "IRAG extendida")) %>%
  
  summarise(
    n_IRAGE = sum (CLASIFICACION_MANUAL == "IRAG extendida"),
    n_Total = n()) %>%
  
  mutate(pct = round(n_IRAGE/n_Total *100.1)) %>%
  pull (pct)

pct_IRAGE
