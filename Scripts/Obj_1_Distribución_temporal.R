#========================================================================
# Objetivo 1:Distribución temporal de los casos de IRAG  e IRAG extendida 
#=========================================================================

#===============================================
# ===== CASOS POR SE Y ANIO =====
#===============================================

#Agrupo casos por año y semana epidemiológica
casos_semana_anio <- data %>% group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 


# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_semana_anio <- casos_semana_anio %>% pivot_wider(names_from = CLASIFICACION_MANUAL,
                                                       values_from = CASOS,
                                                       
                                                       values_fill = 0) 
#====================================================
# 📊 Curva interactiva casos de irag e irage por SE
#===================================================

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
    color = "#C44228") 

curva_interactiva
