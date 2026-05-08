# ==================================================================
# SCRIPTS 3 - LIMPIEZA
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS LIMPIEZA
# ==================================================================


data <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )
