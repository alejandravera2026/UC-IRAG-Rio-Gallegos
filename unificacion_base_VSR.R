

library(dplyr)

# 1. Histórico: solo 2024 y 2025 del primer archivo
data_historico <- data %>%
  filter(ANIO_MIN_INTERNACION < 2026)

# 2. Actual: todo 2026 hasta SE 32 del segundo archivo
data_actual <- data1 %>%
  filter(ANIO_MIN_INTERNACION == 2026 & SEPI_MIN_INTERNACION <= 32)

# 3. Unificación final sin duplicados
base_final <- bind_rows(data_historico, data_actual) %>%
  distinct() # por si hay filas exactamente iguales repetidas


#PERIODO DE ANALISIS

ANIO_MINIMO <- 2024

SEMANA_MINIMA <- 18

ANIO_MAXIMO <- 2026

SEMANA_MAXIMA <- 32


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



#1- CREACIÓN DE GRUPOS ETARIOS SEGÚN LA VARIABLE EDAD_UCIRAG  ------------
  # Grupos: 0 a 3 meses; 4 a 6 meses; 7 a 11 meses
  
table(base_final$EDAD_UC_IRAG)


  base_final_vsr <- base_final %>%
  mutate(
    grupo_etario = case_when(
      EDAD_UC_IRAG == "0 a 2 Meses"  ~ "0 a 2 Meses",
      EDAD_UC_IRAG == "3 a 5 Meses" ~ "3 a 5 Meses",
      EDAD_UC_IRAG == "6 a 11 Meses" ~ "6 a 11 Meses",
      TRUE ~ NA_character_
    )
  )

## # 1- SELECCIONO VARIABLES PARA ANALIZAR  ----------------------------------
  
  distribucion_grupo_etario_vsr <- base_final_vsr %>%
    select(CLASIFICACION_MANUAL, grupo_etario, SEPI)
  
  
  # 2- GRUPO ETARIO de 0 a 2 meses ------------------------------------------------
  
  de_0_a_2_meses <- distribucion_grupo_etario_vsr %>%
    filter(
      grupo_etario == "0 a 2 Meses",
      CLASIFICACION_MANUAL %in% c("IRAG extendida", "Infección respiratoria aguda grave (IRAG)"))
  
  
  casos_0_a_2_meses <- de_0_a_2_meses %>%
    group_by(SEPI,CLASIFICACION_MANUAL) %>%
    summarise(
      CASOS = n(),
      .groups = "drop"
    ) %>%
    arrange(SEPI) 
  
  #3' Paso datos a formato ancho (wider) para hacer curva interactiva
  
  casos_0_a_2_meses <- casos_0_a_2_meses %>% 
    pivot_wider(names_from = CLASIFICACION_MANUAL,
                values_from = CASOS,
                values_fill = 0) 
  
  # 4- CURVA INTERACTIVA  ---------------------------------------------------
  
  curva_interactiva_0_a_2_meses <- highchart() %>%
    
    hc_chart(type = "column") %>%
    
    hc_title(
      text = "Casos de IRAG e IRAG extendida de 0 a 2 meses"
    ) %>%
    
    hc_subtitle(
      text = "Unidad Centinela HRRG, 2024–2026"
    ) %>%
    
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0
      )
    ) %>%
    
    hc_xAxis(
      categories = casos_0_a_2_meses$SEPI,
      title = list(text = NULL),
      
      labels = list(
        rotation = -45,
        step = 2
      )
    ) %>%
    
    hc_yAxis(
      title = list(text = "Número de casos"),
      gridLineColor = "#E6E6E6"
    ) %>%
    
    hc_add_series(
      data = casos_0_a_2_meses$`Infección respiratoria aguda grave (IRAG)`,
      name = "IRAG",
      color = "#252C61"
    ) %>%
    
    hc_add_series(
      data = casos_0_a_2_meses$`IRAG extendida`,
      name = "IRAG extendida",
      color = "#7EC8E6"
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      pointFormat = paste0(
        "<span style='color:{point.color}'>●</span> ",
        "{series.name}: <b>{point.y}</b><br/>"
      )
    ) %>%
    
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      
      itemStyle = list(
        fontWeight = "normal",
        fontSize = "11px"
      )
    )
  
  curva_interactiva_0_a_2_meses
  
  # 4- GRUPO ETARIO DE 3 a 5 meses -----------------------------------------
  
  de_3_a_5_meses <- distribucion_grupo_etario_vsr %>%
    filter(
      grupo_etario == "0 a 2 Meses",
      CLASIFICACION_MANUAL %in% c("IRAG extendida", "Infección respiratoria aguda grave (IRAG)"))
  
  
  casos_3_a_5_meses <- de_3_a_5_meses %>%
    group_by(SEPI,CLASIFICACION_MANUAL) %>%
    summarise(
      CASOS = n(),
      .groups = "drop"
    ) %>%
    arrange(SEPI) 
  
  #3' Paso datos a formato ancho (wider) para hacer curva interactiva
  
  casos_3_a_5_meses <- casos_3_a_5_meses%>% 
    pivot_wider(names_from = CLASIFICACION_MANUAL,
                values_from = CASOS,
                values_fill = 0) 
  
  
  
  
  # 5- CURVA INTERACTIVA - IRAG /IRAG EXTENDIDA EN 3 a 5 meses --------------------
  
  curva_interactiva_3_a_5_meses <- highchart() %>%
    
    hc_chart(type = "column") %>%
    
    hc_title(
      text = "Casos de IRAG e IRAG extendida de 3 a 5 meses"
    ) %>%
    
    hc_subtitle(
      text = "Unidad Centinela HRRG, 2024–2026"
    ) %>%
    
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0
      )
    ) %>%
    
    hc_xAxis(
      categories = casos_3_a_5_meses$SEPI,
      title = list(text = NULL),
      
      labels = list(
        rotation = -45,
        step = 2
      )
    ) %>%
    
    hc_yAxis(
      title = list(text = "Número de casos"),
      gridLineColor = "#E6E6E6"
    ) %>%
    
    hc_add_series(
      data = casos_3_a_5_meses$`Infección respiratoria aguda grave (IRAG)`,
      name = "IRAG",
      color = "#252C61"
    ) %>%
    
    hc_add_series(
      data = casos_3_a_5_meses$`IRAG extendida`,
      name = "IRAG extendida",
      color = "#7EC8E6"
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      pointFormat = paste0(
        "<span style='color:{point.color}'>●</span> ",
        "{series.name}: <b>{point.y}</b><br/>"
      )
    ) %>%
    
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      
      itemStyle = list(
        fontWeight = "normal",
        fontSize = "11px"
      )
    )
  
  curva_interactiva_3_a_5_meses

  
#CASOS DE 6 A 11 MESES
  
  de_6_a_11_meses <- distribucion_grupo_etario_vsr %>%
    filter(
      grupo_etario == "6 a 11 Meses",
      CLASIFICACION_MANUAL %in% c("IRAG extendida", "Infección respiratoria aguda grave (IRAG)"))
  
  
  casos_6_a_11_meses <- de_6_a_11_meses %>%
    group_by(SEPI,CLASIFICACION_MANUAL) %>%
    summarise(
      CASOS = n(),
      .groups = "drop"
    ) %>%
    arrange(SEPI) 
  
  #3' Paso datos a formato ancho (wider) para hacer curva interactiva
  
  casos_6_a_11_meses <- casos_6_a_11_meses%>% 
    pivot_wider(names_from = CLASIFICACION_MANUAL,
                values_from = CASOS,
                values_fill = 0) 
  
  
  
  
  # 5- CURVA INTERACTIVA - IRAG /IRAG EXTENDIDA EN 6 a 11 meses --------------------
  
  curva_interactiva_6_a_11_meses <- highchart() %>%
    
    hc_chart(type = "column") %>%
    
    hc_title(
      text = "Casos de IRAG e IRAG extendida de 6 a 11 meses"
    ) %>%
    
    hc_subtitle(
      text = "Unidad Centinela HRRG, 2024–2026"
    ) %>%
    
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0
      )
    ) %>%
    
    hc_xAxis(
      categories = casos_6_a_11_meses$SEPI,
      title = list(text = NULL),
      
      labels = list(
        rotation = -45,
        step = 2
      )
    ) %>%
    
    hc_yAxis(
      title = list(text = "Número de casos"),
      gridLineColor = "#E6E6E6"
    ) %>%
    
    hc_add_series(
      data = casos_6_a_11_meses$`Infección respiratoria aguda grave (IRAG)`,
      name = "IRAG",
      color = "#252C61"
    ) %>%
    
    hc_add_series(
      data = casos_6_a_11_meses$`IRAG extendida`,
      name = "IRAG extendida",
      color = "#7EC8E6"
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      pointFormat = paste0(
        "<span style='color:{point.color}'>●</span> ",
        "{series.name}: <b>{point.y}</b><br/>"
      )
    ) %>%
    
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      
      itemStyle = list(
        fontWeight = "normal",
        fontSize = "11px"
      )
    )
  
  curva_interactiva_6_a_11_meses

###### POSITIVIDAD VSR===============================
  
  virus_base_vsr <- base_final_vsr %>% 
    select(
      SEPI,
      VSR_FINAL,
      INFLUENZA_FINAL,
      COVID_19_FINAL
    )
  
  
  #============================================================================
  # 2- CONVERSIÓN A FORMATO LARGO
  #============================================================================
  
  virus_largo_vsr <- virus_base_vsr %>%
    
    pivot_longer(
      cols = c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
      names_to = "Agente",
      values_to = "resultado"
    ) %>%
    
    filter(
      !is.na(resultado),
      resultado != "Sin resultado"
    ) %>%
    
    mutate(
      Agente = case_when(
        Agente == "INFLUENZA_FINAL" ~ "Influenza",
        Agente == "VSR_FINAL" ~ "VSR",
        Agente == "COVID_19_FINAL" ~ "SARS-CoV-2",
        TRUE ~ NA_character_
      ),
      
      CATEGORIA = case_when(
        resultado == "Negativo" ~ "Negativos",
        resultado != "Negativo" & Agente == "Influenza" ~ "Influenza",
        resultado != "Negativo" & Agente == "VSR" ~ "VSR",
        resultado != "Negativo" & Agente == "SARS-CoV-2" ~ "SARS-CoV-2",
        TRUE ~ NA_character_
      )
    )
  #============================================================================
  # 3- RECUENTO DE DETERMINACIONES POR SEMANA Y CATEGORÍA
  #============================================================================
  
  virus_semanal_vsr <- virus_largo_vsr %>%
    
    group_by(SEPI, CATEGORIA) %>%
    
    summarise(
      N = n(),
      .groups = "drop"
    )
  
  #============================================================================
  # 4- FORMATO ANCHO PARA EL GRÁFICO
  #============================================================================
  
  virus_semanal_grafico_vsr <- virus_semanal_vsr %>%
    
    pivot_wider(
      names_from = CATEGORIA,
      values_from = N,
      values_fill = 0
    )
  #============================================================================
  # 5- ASEGURAR QUE EXISTAN TODAS LAS COLUMNAS
  #============================================================================
  
  if(!"Influenza" %in% names(virus_semanal_grafico_vsr)
  ) virus_semanal_grafico_vsr$Influenza <- 0
  if(!"VSR" %in% names(virus_semanal_grafico_vsr)
  ) virus_semanal_grafico_vsr$VSR <- 0
  if(!"SARS-CoV-2" %in% names(virus_semanal_grafico_vsr)
  ) virus_semanal_grafico$`SARS-CoV-2` <- 0
  if(!"Negativos" %in% names(virus_semanal_grafico_vsr)
  ) virus_semanal_grafico_vsr$Negativos <- 0
  
  #============================================================================
  # 6- GRÁFICO DE BARRAS APILADAS POR SEMANA EPIDEMIOLÓGICA
  #============================================================================
  
  grafico_virus_semanal_apilado_vsr <- highchart() %>%
    
    hc_chart(type = "column") %>%
    
    hc_title(
      text = "Determinación para Virus Sincicial Respiratorio en niños menores de 11 meses"
      ) %>%
    
    hc_subtitle(
      text = "Resultados positivos para VSR
    y determinaciones negativas. Unidad Centinela HRRG, 2024–2026"
    ) %>%
    
    hc_xAxis(
      categories = virus_semanal_grafico_vsr$SEPI,
      title = list(text = NULL),
      labels = list(
        rotation = -45,
        step = 8
      )
    ) %>%
    
    hc_yAxis(
      min = 0,
      title = list(text = "Número de determinaciones"),
      reversedStacks = FALSE,
      gridLineColor = "#E6E6E6"
    ) %>%
    
    hc_plotOptions(
      column = list(
        stacking = "normal",
        borderWidth = 0,
        pointPadding = 0.05,
        groupPadding = 0.05,
        dataLabels = list(
          enabled = FALSE
        )
      )
    ) %>%
    
    hc_add_series(
      name = "VSR",
      data = virus_semanal_grafico_vsr$VSR,
      color = "#009E73"
    ) %>%
    
    
    hc_add_series(
      name = "Negativos",
      data = virus_semanal_grafico_vsr$Negativos,
      color = "#D9D9D9"
    ) %>%
    
    hc_tooltip(
      shared = TRUE
    )
  
  grafico_virus_semanal_apilado_vsr

  
  #===============================================================================
  # 1. Selección de variables y construcción de grupos de riesgo
  #===============================================================================

  
  table(base_final_vsr$VAC_VSR)
  
  
  vacunada <- c("OTRA SE", "SE 32", "SE 34", "SE 35", "SE 36", "SE DESCONOCIDA")
  
  no_vacunada <- c("MADRE NO VACUNADA")
  
  
  # 5- APLICO NUEVAS CATEGORÍAS A LAS VACUNAS VSR MATERNA ------------
    
  vacunacion_vsr <- base_final_vsr %>%
    select(
      EDAD_UC_IRAG,
      VAC_VSR
    ) %>%
    mutate(
      grupo_riesgo_vacunacion = case_when(
        EDAD_UC_IRAG %in% c("0 a 2 Meses") ~ "De 0 a 2 meses",
        EDAD_UC_IRAG %in% c("3 a 5 Meses") ~ "De 3 a 5 meses",
        EDAD_UC_IRAG %in% c("6 a 11 Meses") ~ 
          "De 6 a 11 meses",
        TRUE ~ NA_character_
      ),
      grupo_riesgo_vacunacion = factor(
        grupo_riesgo_vacunacion,
        levels = c(
          "De  0 a 2 meses",
          "De 3 a 5 meses",
          "De 6 a 11 meses"
        )
      )
    ) %>%
    filter(!is.na(grupo_riesgo_vacunacion))
  
  
  #===============================================================================
  # 2. PARTE 1 - VACUNACIÓN MATERNA EN PACIENTES MENORES DE 11 MESES
  #===============================================================================
  
  vacunacion_materna_larga_vsr <- vacunacion_vsr %>%
    filter(grupo_riesgo_vacunacion %in% c ("De 0 a 2 meses", "De 3 a 5 meses", 
                                           " De 6 a 11 meses")) %>%
    select(
      `VSR materna` = VAC_VSR
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Vacuna",
      values_to = "Estado"
    ) %>%
    mutate(
      Estado = case_when(
        Estado %in% c("vacunada") ~ "Vacunada",
        Estado %in% c("No_vacunada") ~ "No_vacunada",
        TRUE ~ "SIN DATO"
      ),
      Estado = factor(
        Estado,
        levels = c("Vacunada", "No_vacunada", "SIN DATO")
      )
    )

  
  #-------------------------------------------------------------------------------
  # Tabla 1 - Vacunación vsr materna
  #-------------------------------------------------------------------------------
  
  tabla_vsr_materna <- vacunacion_materna_larga_vsr %>%
    filter(Vacuna == "VSR materna") %>%
    count(Estado, name = "casos") %>%
    mutate(
      porcentaje = round(casos / sum(casos) * 100, 1),
      resultado = paste0(casos, " (", porcentaje, "%)")
    ) %>%
    select(Estado, resultado) %>%
    gt() %>%
    cols_label(
      Estado = "Estado",
      resultado = "Resultado"
    ) %>%
    cols_align(
      align = "center"
    ) %>%
    tab_header(
      title = "Vacunación VSR materna.",
      subtitle = "Pacientes menores de 11 meses. Unidad Centinela HRRG, 2024–2026."
    )
  
  tabla_vsr_materna
  
  
  #-------------------------------------------------------------------------------
  # Tabla 2 - Vacunación materna contra VSR
  #-------------------------------------------------------------------------------
  
  tabla_vsr_materna <- vacunacion_materna_larga %>%
    filter(Vacuna == "VSR materna") %>%
    count(Estado, name = "casos") %>%
    mutate(
      porcentaje = round(casos / sum(casos) * 100, 1),
      resultado = paste0(casos, " (", porcentaje, "%)")
    ) %>%
    select(Estado, resultado) %>%
    gt() %>%
    cols_label(
      Estado = "Estado",
      resultado = "Resultado"
    ) %>%
    cols_align(
      align = "center"
    ) %>%
    tab_header(
      title = "Vacunación materna contra VSR.",
      subtitle = "Pacientes menores de 6 meses. Unidad Centinela HRRG, 2024–2026."
    )
  
  tabla_vsr_materna
  
  
  
  #===============================================================================
  # 3. PARTE 2 - VACUNACIÓN ANTIGRIPAL EN GRUPOS DE RIESGO
  #===============================================================================
  
  vacunacion_antigripal_larga <- vacunacion_base %>%
    filter(grupo_riesgo_vacunacion %in% c("6 a 23 meses", "65 años y más")) %>%
    select(
      grupo_riesgo_vacunacion,
      VAC_ANTIGRIPAL
    ) %>%
    mutate(
      Estado = case_when(
        VAC_ANTIGRIPAL =="VACUNADO" ~ "Vacunado",
        VAC_ANTIGRIPAL %in% c("NO VACUNADA", "NO VACUNADO") ~ "No vacunado",
        TRUE ~ "Sin dato"
      ))
  
  
  tabla_antigripal_riesgo <- vacunacion_antigripal_larga %>%
    count(grupo_riesgo_vacunacion, Estado, name = "casos") %>%
    group_by(grupo_riesgo_vacunacion) %>%
    mutate(
      porcentaje = round(casos / sum(casos) * 100, 1),
      etiqueta = paste0(round(porcentaje, 1), "%")
    ) %>%
    ungroup()
  
  #===============================================================================
  # Gráfico - vacunación antigripal en grupos de riesgo
  #===============================================================================
  
  tabla_antigripal_high <- tabla_antigripal_riesgo %>%
    mutate(
      Estado = factor(
        Estado,
        levels = c("Vacunado", "No vacunado", "Sin dato")
      )
    )
  
  
  grafico_antigripal_riesgo <- highchart() %>%
    
    hc_chart(type = "bar") %>%
    
    hc_title(
      text = "Vacunación antigripal en grupos de riesgo"
    ) %>%
    
    hc_subtitle(
      text = "Unidad Centinela HRRG, 2024–2026"
    ) %>%
    
    hc_xAxis(
      categories = c("6 a 23 meses", "65 años y más"),
      title = list(text = NULL)
    ) %>%
    
    hc_yAxis(
      title = list(text = "Porcentaje (%)"),
      labels = list(format = "{value}%"),
      min = 0,
      max = 100,
      tickInterval = 20,
      gridLineColor = "#E6E6E6",
      reversedStacks = FALSE
    ) %>%
    
    hc_plotOptions(
      series = list(
        stacking = "normal",
        
        dataLabels = list(
          enabled = TRUE,
          format = "{point.y:.1f}%",
          style = list(
            color = "white",
            fontWeight = "bold",
            textOutline = "none"
          )
        )
      )
    ) %>%
    
    hc_add_series(
      name = "Vacunado",
      
      data = tabla_antigripal_high %>%
        filter(Estado == "Vacunado") %>%
        pull(porcentaje),
      
      color = "#2e7d32"
    ) %>%
    
    hc_add_series(
      name = "No vacunado",
      
      data = tabla_antigripal_high %>%
        filter(Estado == "No vacunado") %>%
        pull(porcentaje),
      
      color = "#e67d32"
    ) %>%
    
    hc_tooltip(
      shared = TRUE,
      
      pointFormat = paste0(
        "<span style='color:{point.color}'>●</span> ",
        "{series.name}: <b>{point.y:.1f}%</b><br/>"
      )
    ) %>%
    
    hc_legend(
      align = "center",
      verticalAlign = "bottom",
      
      itemStyle = list(
        fontWeight = "normal",
        fontSize = "11px"
      )
    )
  
  grafico_antigripal_riesgo
  
  
  
  