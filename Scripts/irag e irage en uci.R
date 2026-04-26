#========================================================
# OBJETIVO 7
# Estimar la proporción de IRAG e IRAGE
# que requieren ingreso a cuidados intensivos (UCI)
# por semana epidemiológica
#========================================================

#--------------------------------------------------------
# 1. Parámetros del período de análisis
#--------------------------------------------------------

ANIO_MINIMO <- 2024
SEMANA_MINIMA <- 18
ANIO_MAXIMO <- 2026
SEMANA_MAXIMA <- 8

#--------------------------------------------------------
# 2. Limpieza inicial
#--------------------------------------------------------

data <- data %>%
  filter(
    CLASIFICACION_MANUAL != "Caso invalidado por epidemiología"
  )

#--------------------------------------------------------
# 3. Selección de variables
#--------------------------------------------------------

uci <- data %>%
  select(
    CLASIFICACION_MANUAL,
    CUIDADO_INTENSIVO,
    FECHA_CUI_INTENSIVOS,
    SEPI_MIN_INTERNACION,
    ANIO_MIN_INTERNACION
  )

#--------------------------------------------------------
# 4. Filtro por período de estudio
#--------------------------------------------------------

uci <- uci %>%
  filter(
    (
      ANIO_MIN_INTERNACION > ANIO_MINIMO |
        (
          ANIO_MIN_INTERNACION == ANIO_MINIMO &
            SEPI_MIN_INTERNACION >= SEMANA_MINIMA
        )
    ) &
      (
        ANIO_MIN_INTERNACION < ANIO_MAXIMO |
          (
            ANIO_MIN_INTERNACION == ANIO_MAXIMO &
              SEPI_MIN_INTERNACION <= SEMANA_MAXIMA
          )
      )
  )

#========================================================
#====================== IRAG ============================
#========================================================

uci_irag <- uci %>% # Agrupo casos de IRAG por condición de UCI
  filter(
    CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)"
  ) %>%
  group_by(
    ANIO_MIN_INTERNACION,
    SEPI_MIN_INTERNACION,
    CUIDADO_INTENSIVO
  ) %>%
  summarise(
    Casos = n(),
    .groups = "drop"
  )

#------------------ Grafico proporción de IRAG por semana ------------------------

ggplot(
  uci_irag %>%
    filter(CUIDADO_INTENSIVO %in% c("SI", "NO")),
  aes(
    x = SEPI_MIN_INTERNACION,
    y = Casos,
    fill = CUIDADO_INTENSIVO
  )
) +
  geom_col(position = "fill") +
  facet_wrap(~ANIO_MIN_INTERNACION) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de internados en UCI por IRAG por semana epidemiológica",
    subtitle = "Infección Respiratoria Aguda Grave",
    x = "Semana epidemiológica",
    y = "Proporción",
    fill = "Cuidado Intensivo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

#========================================================
#=================== IRAG EXTENDIDA =====================
#========================================================

uci_irage <- uci %>% # Agrupo casos de IRAGE por condición de UCI
  filter(
    CLASIFICACION_MANUAL == "IRAG extendida"
  ) %>%
  group_by(
    ANIO_MIN_INTERNACION,
    SEPI_MIN_INTERNACION,
    CUIDADO_INTENSIVO
  ) %>%
  summarise(
    Casos = n(),
    .groups = "drop"
  )

#---------------- Grafico IRAG Extendida por semana ----------------

ggplot(
  uci_irage %>%
    filter(CUIDADO_INTENSIVO %in% c("SI", "NO")),
  aes(
    x = SEPI_MIN_INTERNACION,
    y = Casos,
    fill = CUIDADO_INTENSIVO
  )
) +
  geom_col(position = "fill") +
  facet_wrap(~ANIO_MIN_INTERNACION) +
  scale_y_continuous(labels = percent) +
  labs(
    title = "Proporción de internados en UCI por IRAGE por semana epidemiológica",
    subtitle = "IRAG Extendida",
    x = "Semana epidemiológica",
    y = "Proporción",
    fill = "Cuidado Intensivo"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

#========================================================
# OPCIÓN EXTRA: IRAG
# Gráfico interactivo con Highcharter
#========================================================

highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_plotOptions(
    column = list(
      stacking = "percent"
    )
  ) %>%
  
  # Primero NO (queda arriba)
  hc_add_series(
    name = "NO",
    data = uci_irag %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      filter(CUIDADO_INTENSIVO == "NO") %>%
      pull(Casos)
  ) %>%
  
  # Después SI (queda abajo)
  hc_add_series(
    name = "SI",
    data = uci_irag %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      filter(CUIDADO_INTENSIVO == "SI") %>%
      pull(Casos)
  ) %>%
  
  hc_xAxis(
    categories = uci_irag %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      distinct(SEPI_MIN_INTERNACION) %>%
      pull(SEPI_MIN_INTERNACION),
    
    title = list(
      text = "Semana epidemiológica"
    )
  ) %>%
  
  hc_yAxis(
    title = list(
      text = "Proporción"
    )
  ) %>%
  
  hc_title(
    text = "Proporción de internados en UCI por IRAG"
  ) %>%
  
  hc_subtitle(
    text = "Gráfico interactivo"
  )

#========================================================
# OPCIÓN EXTRA: IRAGE
# Gráfico interactivo con Highcharter
#========================================================

highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_plotOptions(
    column = list(
      stacking = "percent"
    )
  ) %>%
  
  # Primero NO (queda arriba)
  hc_add_series(
    name = "NO",
    data = uci_irage %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      filter(CUIDADO_INTENSIVO == "NO") %>%
      pull(Casos)
  ) %>%
  
  # Después SI (queda abajo)
  hc_add_series(
    name = "SI",
    data = uci_irag %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      filter(CUIDADO_INTENSIVO == "SI") %>%
      pull(Casos)
  ) %>%
  
  hc_xAxis(
    categories = uci_irag %>%
      arrange(SEPI_MIN_INTERNACION) %>%
      distinct(SEPI_MIN_INTERNACION) %>%
      pull(SEPI_MIN_INTERNACION),
    
    title = list(
      text = "Semana epidemiológica"
    )
  ) %>%
  
  hc_yAxis(
    title = list(
      text = "Proporción"
    )
  ) %>%
  
  hc_title(
    text = "Proporción de internados en UCI por IRAG"
  ) %>%
  
  hc_subtitle(
    text = "Gráfico interactivo"
  )

