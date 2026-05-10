# ===========================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ===========================================================================


# 1- ELIMINO "Casos invalidados por epidemiología" ------------------------

data1 <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )


# 2- CREO VARIABLE SEPI ---------------------------------------------------

data1 <- data1 %>%
  mutate(
    SEPI= paste(
    ANIO_MIN_INTERNACION,
    "-",
    str_pad(SEPI_MIN_INTERNACION,2,pad=
              "0")
    )
    )


# 3- FILTRO PERIODO A ANALIZAR  -------------------------------------------

data1 <- data1 %>%
  filter(
    (ANIO_MIN_INTERNACION >
       ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == 
          ANIO_MINIMO & 
          SEPI_MIN_INTERNACION >= 
          SEMANA_MINIMA)
     ) &
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO |
         (ANIO_MIN_INTERNACION == 
            ANIO_MAXIMO & 
            SEPI_MIN_INTERNACION <= 
            SEMANA_MAXIMA)
       )
    )


# 4- CREO BASE ANALÍTICA PRINCIPAL LIMPIA ---------------------------------
# data = base original trabajada
# data_principal = base analítica limpia

data_principal <- data1

# 5- CONTROL FINAL DE BASE ANALÍTICA --------------------------------------

glimpse(data_principal)

dim(data_principal)

table(data_principal$CLASIFICACION_MANUAL)

table(data_principal$ANIO_MIN_INTERNACION)

str(data_principal)


