#========================================================================
# Objetivo 1:Distribución temporal de los casos de IRAG  e IRAG extendida 
#=========================================================================

# Primero se visualiza lo que figura en Clasificación Manual utilizando la función
#unique
  
unique(data$CLASIFICACION_MANUAL)

# Se elimina casos invalidados por epidemiología utilizando función filter

data <- data %>% filter (CLASIFICACION_MANUAL != "Caso invalidado por epidemiología")

# Completo semanas epidemiológicas y creo variable SEPI (une año con SE)

data <- data %>% complete(ANIO_MIN_INTERNACION,
                          SEPI_MIN_INTERNACION = 1:52,
                          fill = list(n = 0)) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-",str_pad(SEPI_MIN_INTERNACION,2,pad= "0")))

#=========================================
# ===== PARAMETROS TEMPORALES =====
#=========================================

#Se convierte a variable a numérica a las variables ANIO_MIN_INTERNACION Y SEPI_MIN_INTERNACION

data$ANIO_FECHA_MINIMA <- as.numeric (data$ANIO_MIN_INTERNACION)

data$SEPI_FECHA_MINIMA <- as.numeric (data$SEPI_MIN_INTERNACION)

#===============================================
# Aplico filtros respetanto periodo de análisis
#==============================================
data <- data %>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & SEPI_MIN_INTERNACION <= SEMANA_MAXIMA))
  )

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
