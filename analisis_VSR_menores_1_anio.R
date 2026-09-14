# Análisis de casos confirmados de VSR en menores de un año. Unidad Centinela HRRG
# Período: SE 18/2024 a SE 32/2026

library(dplyr)

# Este script utiliza el objeto base_final,
# generado previamente por el script de unificación.

# 1. Selección de casos positivos de VSR en menores de un año ----

casos_vsr_menores1 <- base_final %>%
  mutate(
    edad_meses_unificada = coalesce(EDAD_MESES, edad_meses)
  ) %>%
  filter(
    # Período: SE 18/2024 a SE 32/2026
    (ANIO_MIN_INTERNACION == 2024 & SEPI_MIN_INTERNACION >= 18) |
      ANIO_MIN_INTERNACION == 2025 |
      (ANIO_MIN_INTERNACION == 2026 & SEPI_MIN_INTERNACION <= 32),
    
    # Clasificación clínica
    CLASIFICACION_MANUAL %in% c(
      "Infección respiratoria aguda grave (IRAG)",
      "IRAG extendida"
    ),
    
    # Resultado positivo
    VSR_FINAL %in% c("VSR", "VSR A", "VSR B"),
    
    # Edad de 0 a 11 meses
    between(edad_meses_unificada, 0, 11)
  ) %>%
  mutate(
    grupo_etario_solicitado = case_when(
      edad_meses_unificada < 4 ~ "0–3 meses",
      edad_meses_unificada < 7 ~ "4–6 meses",
      TRUE ~ "7–11 meses"
    )
  )

# 2. Control de identificadores de evento ----

casos_vsr_menores1 %>%
  summarise(
    registros = n(),
    identificadores_unicos = n_distinct(
      IDEVENTOCASO[!is.na(IDEVENTOCASO)]
    ),
    identificadores_faltantes = sum(is.na(IDEVENTOCASO)),
    registros_con_id_repetido = sum(
      duplicated(IDEVENTOCASO[!is.na(IDEVENTOCASO)])
    )
  )

# 3. Revisión del antecedente de vacunación materna por año ----

casos_vsr_menores1 %>%
  count(
    ANIO_MIN_INTERNACION,
    VAC_VSR,
    name = "cantidad"
  ) %>%
  arrange(ANIO_MIN_INTERNACION, VAC_VSR) %>%
  as_tibble() %>%
  print(n = Inf)

# 4. Antecedente de vacunación materna y semana gestacional ----

casos_vsr_menores1 <- casos_vsr_menores1 %>%
  mutate(
    antecedente_vacunacion_materna = case_when(
      VAC_VSR %in% c("SE 32", "SE 36") ~ "Madre vacunada",
      VAC_VSR == "MADRE NO VACUNADA" ~ "Madre no vacunada",
      VAC_VSR == "SIN DATO" | is.na(VAC_VSR) ~ "Sin dato",
      TRUE ~ "Revisar"
    ),
    
    semana_gestacional_vacunacion = case_when(
      VAC_VSR == "SE 32" ~ 32L,
      VAC_VSR == "SE 36" ~ 36L,
      TRUE ~ NA_integer_
    )
  )

# Comprobar las categorías
casos_vsr_menores1 %>%
  count(antecedente_vacunacion_materna, name = "cantidad")

# 5. Casos confirmados de VSR por grupo etario y año ----

tabla_casos_edad <- casos_vsr_menores1 %>%
  count(
    grupo_etario_solicitado,
    ANIO_MIN_INTERNACION,
    name = "casos"
  ) %>%
  tidyr::complete(
    grupo_etario_solicitado = c(
      "0–3 meses", "4–6 meses", "7–11 meses"
    ),
    ANIO_MIN_INTERNACION = 2024:2026,
    fill = list(casos = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = ANIO_MIN_INTERNACION,
    values_from = casos
  ) %>%
  mutate(
    Total = `2024` + `2025` + `2026`
  ) %>%
  arrange(grupo_etario_solicitado)

tabla_casos_edad

# 6. Antecedente de vacunación materna por año ----

tabla_vacunacion_anual <- casos_vsr_menores1 %>%
  count(
    antecedente_vacunacion_materna,
    ANIO_MIN_INTERNACION,
    name = "casos"
  ) %>%
  tidyr::complete(
    antecedente_vacunacion_materna = c(
      "Madre vacunada",
      "Madre no vacunada",
      "Sin dato"
    ),
    ANIO_MIN_INTERNACION = 2024:2026,
    fill = list(casos = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = ANIO_MIN_INTERNACION,
    values_from = casos
  ) %>%
  mutate(
    Total = `2024` + `2025` + `2026`,
    antecedente_vacunacion_materna = factor(
      antecedente_vacunacion_materna,
      levels = c(
        "Madre vacunada",
        "Madre no vacunada",
        "Sin dato"
      )
    )
  ) %>%
  arrange(antecedente_vacunacion_materna)

tabla_vacunacion_anual

# 7. Semana gestacional de vacunación materna por año ----

tabla_semana_gestacional <- casos_vsr_menores1 %>%
  filter(antecedente_vacunacion_materna == "Madre vacunada") %>%
  count(
    semana_gestacional_vacunacion,
    ANIO_MIN_INTERNACION,
    name = "casos"
  ) %>%
  tidyr::complete(
    semana_gestacional_vacunacion,
    ANIO_MIN_INTERNACION = 2024:2026,
    fill = list(casos = 0)
  ) %>%
  tidyr::pivot_wider(
    names_from = ANIO_MIN_INTERNACION,
    values_from = casos
  ) %>%
  mutate(
    Total = `2024` + `2025` + `2026`
  ) %>%
  arrange(semana_gestacional_vacunacion)

tabla_semana_gestacional
