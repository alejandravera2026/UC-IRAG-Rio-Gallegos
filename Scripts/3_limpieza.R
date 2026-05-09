# ===========================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ===========================================================================


# Elimina casos invalidados por epidemiología -----------------------------

data <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )


# Completo semanas epidemiológicas y creo variable SEPI -------------------

data <- data %>% complete(ANIO_MIN_INTERNACION,
                          SEPI_MIN_INTERNACION = 1:52,
                          fill = list(n = 0)) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-",
                     str_pad(SEPI_MIN_INTERNACION,2,pad= "0")
                     )
         )


# Normalización de variables y filtrado temporal --------------------------

data$ANIO_FECHA_MINIMA <- as.numeric (data$ANIO_MIN_INTERNACION)

data$SEPI_FECHA_MINIMA <- as.numeric (data$SEPI_MIN_INTERNACION)


# Verifico si las variables estan como NUMÉRICAS --------------------------

str(data$ANIO_MIN_INTERNACION)
str(data$SEPI_MIN_INTERNACION)


# Filtro período a analizar -----------------------------------------------

data <- data %>%
  filter(
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & 
          SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & 
            SEPI_MIN_INTERNACION <= SEMANA_MAXIMA)))
    
    
    
    
    
str(data)


# Creo una nueva base  ----------------------------------------------------

data_principal <- data%>%
  select()

