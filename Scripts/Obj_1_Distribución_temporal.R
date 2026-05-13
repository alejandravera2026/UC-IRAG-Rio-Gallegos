#============================================================================
# SCRIPT 5 - OBJETIVO 1 - DISTRIBUCIÓN TEMPORAL 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- BASE DE TRABAJO ------------------------------------------------------

# Para este objetivo se utiliza la base analítica que se generó 
# en el SCRIPTS 4

data_obj1 <- data


# CASOS POR SE Y ANIO -----------------------------------------------------
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


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de IRAG e IRAG extendida")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_semana_anio$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_semana_anio$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_semana_anio$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva
