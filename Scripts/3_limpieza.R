# ===========================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ===========================================================================


# 1- ELIMINO "Casos invalidados por epidemiología" ------------------------

data <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )


# 2- CREO VARIABLE SEPI ---------------------------------------------------

data <- data %>%
  mutate(
    SEPI= paste(
    ANIO_MIN_INTERNACION,
    "-",
    str_pad(SEPI_MIN_INTERNACION,2,pad=
              "0")
    )
    )


# 3- FILTRO PERIODO A ANALIZAR  -------------------------------------------

data <- data %>%
  filter(
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & 
          SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & 
            SEPI_MIN_INTERNACION <= SEMANA_MAXIMA)))
    
    
    
    
    
str(data)


# 4- CREO BASE ANALÍTICA PRINCIPAL LIMPIA ---------------------------------
# data = base original trabajada
# data_principal = base analítica limpia

data <- data

# 5- CONTROL FINAL DE BASE ANALÍTICA --------------------------------------

glimpse(data)

dim(data)

table(data$CLASIFICACION_MANUAL)

table(data$ANIO_MIN_INTERNACION)

str(data)



