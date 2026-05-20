#============================================================================
# SCRIPT 5 - OBJETIVO 1 - DISTRIBUCIÓN TEMPORAL 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- SE CREA UN NUEVO OBJETO ----------------------------------------------

data_obj1 <- data

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
  
  hc_chart(type= "column") %>%
  
  hc_title(
    text = "Distribución semanal de IRAG e IRAG extendida")%>%
  hc_subtitle(
    text = "Casos notificados según semana epidemiológica. 
    Unidad centinela HRRG, 2024 - 2026."
  )%>%
  
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_semana_anio$SEPI, #categorías en eje X
    title = list(text = NULL)
    ) %>%  
  
  hc_yAxis(title= list(text="Números de casos")
           ) %>%
  
  hc_add_series(
    data = casos_semana_anio$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  
  hc_add_series(
    data = casos_semana_anio$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva



# 3- TOTALES Y PORCENTAJES ------------------------------------------------

data %>%
  count(CLASIFICACION_MANUAL)

notificaciones_totales <- data %>%
  
  filter(CLASIFICACION_MANUAL %in%
           c("Infección respiratoria aguda grave (IRAG)",
             "IRAG extendida")
         )%>%
  summarise(Total = n())

notificaciones_totales

notificaciones_irag <- data %>%
  
  filter(CLASIFICACION_MANUAL %in%
           "Infección respiratoria aguda grave (IRAG)") %>%
  summarise(IRAG = n()
            )

notificaciones_irag

notificaciones_irage <- data %>%
  
  filter(CLASIFICACION_MANUAL %in% "IRAG extendida") %>%
  summarise(IRAGE = n()
            )

notificaciones_irage

pct_IRAG <- data %>%
  
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

pct_IRAGE <- data %>%
  
  filter(CLASIFICACION_MANUAL %in% 
           c("Infección respiratoria aguda grave (IRAG)",
             "IRAG extendida")) %>%
  
  summarise(
    n_IRAGE = sum (CLASIFICACION_MANUAL == "IRAG extendida"),
    n_Total = n()) %>%
  
  mutate(pct = round(n_IRAGE/n_Total *100.1)) %>%
  pull (pct)

pct_IRAGE
