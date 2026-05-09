# ==================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ==================================================================

#Elimina casos invalidados por epidemiología

data <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )

#=========================================================================
# Completo semanas epidemiológicas y creo variable SEPI (une año con SE)
# ========================================================================

data <- data %>% complete(ANIO_MIN_INTERNACION,
                          SEPI_MIN_INTERNACION = 1:52,
                          fill = list(n = 0)) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-",str_pad(SEPI_MIN_INTERNACION,2,pad= "0")))


#===============================================
# Aplico filtros respetanto periodo de análisis
#==============================================

#Se convierte a variable a numérica a las variables ANIO_MIN_INTERNACION Y SEPI_MIN_INTERNACION

data$ANIO_FECHA_MINIMA <- as.numeric (data$ANIO_MIN_INTERNACION)

data$SEPI_FECHA_MINIMA <- as.numeric (data$SEPI_MIN_INTERNACION)

#Filtra registros según lo definido en el plan de análisis

data <- data %>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & SEPI_MIN_INTERNACION <= SEMANA_MAXIMA))
  )


