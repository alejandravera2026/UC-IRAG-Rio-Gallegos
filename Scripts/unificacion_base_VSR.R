

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
      grupo_etario == "3 a 5 Meses",
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
  # 1.VACUNACION: SE SELECCIONA LOS GRUPOS DE EDAD EN ESTUDIO PARA VER EL ESTADO
  #VACUNAL DE LA MADRE
  #===============================================================================

  
  table(base_final_vsr$VAC_VSR)

  vacunacion_vsr <- base_final_vsr %>%
    filter(EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses", "6 a 11 Meses"))
  
  table(vacunacion_vsr$VAC_VSR)
  
  
  
  library(dplyr)
  library(stringr)
  
  tabla_final_vsr <- vacunacion_vsr %>%
    mutate(
      semana_gestacion = case_when(
        VAC_VSR == "OTRA SE" ~ "Otra semana",
        VAC_VSR == "SE DESCONOCIDA" ~ "Vacunada - semana desconocida",
        str_detect(VAC_VSR, "^SE") ~ str_extract(VAC_VSR, "\\d+"),
        VAC_VSR == "MADRE NO VACUNADA" ~ "No vacunada",
        VAC_VSR == "SIN DATO" ~ "Sin dato",
      )
    ) %>%
    count(semana_gestacion, name = "cantidad") %>%
    arrange(factor(semana_gestacion, levels = c("32","34","35","36","Otra semana",
                                                "Vacunada - semana desconocida","No vacunada",
                                                "Sin dato")))
  
  tabla_final_vsr

  tabla_final_vsr <- vacunacion_vsr %>%
    mutate(
      semana_gestacion = case_when(
        VAC_VSR == "OTRA SE" ~ "Otra semana",
        VAC_VSR == "SE DESCONOCIDA" ~ "Vacunada - semana desconocida",
        str_detect(VAC_VSR, "^SE") ~ str_extract(VAC_VSR, "\\d+"),
        VAC_VSR == "MADRE NO VACUNADA" ~ "No vacunada",
        VAC_VSR == "SIN DATO" ~ "Sin dato"
      )
    ) %>%
    count(semana_gestacion, name = "n") %>%
    mutate(
      total = sum(n),
      porcentaje = round(n/total*100, 1)
    ) %>%
    arrange(factor(semana_gestacion, levels = c("32","34","35","36","Otra semana","Vacunada - semana desconocida","No vacunada","Sin dato"))) %>%
    select(-total)
  
  tabla_final_vsr <- tabla_final_vsr %>%
    gt() %>%
    tab_header(
      title = md("**Estado de vacunación materna para VSR**"),
      subtitle = "Menores de 11 meses internados por IRAG/IRAG extendida -HRRG - n: 106 casos"
    ) %>%
    cols_label(
      semana_gestacion = "Semana de gestación/Estado",
      porcentaje = "%"
    ) %>%
    fmt_number(columns = porcentaje, decimals = 1, dec_mark = ",", sep_mark = ".") %>%
    cols_align(columns = c(n, porcentaje), align = "center")

  tabla_final_vsr  

# Se realiza el total de los casos de IRAG e IRAG extendida en menores de 11 meses
  
vacunacion_vsr %>%
    count(CLASIFICACION_MANUAL)
  
  notificaciones_totales_vsr <- vacunacion_vsr %>%
    
    filter(CLASIFICACION_MANUAL %in%
             c("Infección respiratoria aguda grave (IRAG)",
               "IRAG extendida")
    )%>%
    summarise(Total = n())

notificaciones_totales_vsr  
