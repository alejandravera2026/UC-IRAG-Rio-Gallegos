# ===========================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ===========================================================================


# 1- ELIMINO "Casos invalidados por epidemiología" ------------------------

base_final <- base_final %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )


# 2- CREO VARIABLE SEPI ---------------------------------------------------

base_final <- base_final%>%
  mutate(
    SEPI= paste(
    ANIO_MIN_INTERNACION,
    "-",
    str_pad(SEPI_MIN_INTERNACION,2,pad=
              "0")
    )
    )
#PERIODO DE ANALISIS

ANIO_MINIMO <- 2024

SEMANA_MINIMA <- 18

ANIO_MAXIMO <- 2026

SEMANA_MAXIMA <- 43

# 3- FILTRO PERIODO A ANALIZAR  -------------------------------------------

base_final <- base_final %>%
  filter(
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & 
          SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & 
            SEPI_MIN_INTERNACION <= SEMANA_MAXIMA)))

    
# 4- CREO BASE LIMPIA -----------------------------------------------------

base_limpia <- base_final


# 5- CONTROL FINAL DE BASE ANALÍTICA --------------------------------------

dim(base_limpia)

table(base_limpia$CLASIFICACION_MANUAL)

table(base_limpia$ANIO_MIN_INTERNACION)





