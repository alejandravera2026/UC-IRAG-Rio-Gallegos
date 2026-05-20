#============================================================================
# SCRIPT 6 - OBJETIVO 2 - DISTRIBUCIÓN DE CASOS POR GRUPO ETARIO, SE Y AÑO
# IDENTIFICANDO GRUPOS POBLACIONALES DE MAYOR RIESGO
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
#============================================================================


# 1- SELECCIONO VARIABLES PARA ANALIZAR  ----------------------------------

distribucion_grupo_etario <- data %>%
  select(CLASIFICACION_MANUAL, grupo_etario, SEPI)


# 2- GRUPO ETARIO < 2 AÑOS ------------------------------------------------

menor_dos_anios <- distribucion_grupo_etario %>%
  filter(
    grupo_etario == "< 2 años",
    CLASIFICACION_MANUAL %in% c("IRAG extendida", "Infección respiratoria aguda grave (IRAG)"))
  

casos_menores_dos_anios <- menor_dos_anios %>%
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(
    CASOS = n(),
    .groups = "drop"
  ) %>%
  arrange(SEPI) 

#3' Paso datos a formato ancho (wider) para hacer curva interactiva

casos_menores_dos_anios <- casos_menores_dos_anios %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 
  
# 4- CURVA INTERACTIVA  ---------------------------------------------------

curva_interactiva_menores_dos <- highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Distribución semanal de casos de IRAG e IRAG extendida en menores de 2 años"
  ) %>%
  
  hc_subtitle(
    text = "Casos notificados según semana epidemiológica. Unidad Centinela HRRG, 2024–2026"
  ) %>%
  
  hc_plotOptions(
    column = list(
      pointPadding = 0.1,
      groupPadding = 0.05,
      borderWidth = 0
    )
  ) %>%
  
  hc_xAxis(
    categories = casos_menores_dos_anios$SEPI,
    title = list(text = NULL),
    labels = list(rotation = -45, step = 2)
  ) %>%
  
  hc_yAxis(
    title = list(text = "Número de casos")
  ) %>%
  
  hc_add_series(
    data = casos_menores_dos_anios$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6"
  ) %>%
  hc_add_series(
    data = casos_menores_dos_anios$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61"
  )%>%
  
  hc_legend(enabled = FALSE)

curva_interactiva_menores_dos


# 4- GRUPO ETARIO DE 2 A 14 AÑOS  -----------------------------------------

grupo_2_14 <- distribucion_grupo_etario %>%
  filter(
    grupo_etario == "2-14 años",
    CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)"
  )

casos_2_14 <- grupo_2_14 %>%
  group_by(SEPI) %>%
  summarise(
    CASOS = n(),
    .groups = "drop"
  ) %>%
  arrange(SEPI)

# 5- CURVA INTERACTIVA - IRAG EN POBLACIÓN DE 2 A 14 AÑOS --------------------

curva_interactiva_2_14 <- highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Distribución semanal de casos de IRAG en población de 2 a 14 años"
  ) %>%
  
  hc_subtitle(
    text = "Casos notificados según semana epidemiológica. 
    Unidad Centinela HRRG, 2024–2026."
  ) %>%
  
  hc_plotOptions(
    column = list(
      pointPadding = 0.1,
      groupPadding = 0.05,
      borderWidth = 0
    )
  ) %>%
  
  hc_xAxis(
    categories = casos_2_14$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 2
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "Número de casos")
  ) %>%
  
  hc_add_series(
    data = casos_2_14$CASOS,
    name = "IRAG",
    color = "#252C61"
  )%>% 
  
   hc_legend(enabled = FALSE)

curva_interactiva_2_14


# 6- GRUPO ETARIO 15 A 59 AÑOS - IRAG  ------------------------------------

grupo_15_59 <- distribucion_grupo_etario %>%
  filter(
    grupo_etario == "15-59 años",
    CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)"
  )

casos_15_59 <- grupo_15_59 %>%
  group_by(SEPI) %>%
  summarise(
    CASOS = n(),
    .groups = "drop"
  ) %>%
  arrange(SEPI)


# 7- CURVA INTERACTIVA - IRAG EN POBLACIÓN DE 15 A 59 AÑOS -------------------

curva_interactiva_15_59 <- highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Distribución semanal de casos de IRAG en población de 15 a 59 años"
  ) %>%
  
  hc_subtitle(
    text = "Casos notificados según semana epidemiológica. 
    Unidad Centinela HRRG, 2024–2026."
  ) %>%
  
  hc_plotOptions(
    column = list(
      pointPadding = 0.1,
      groupPadding = 0.05,
      borderWidth = 0
    )
  ) %>%
  
  hc_xAxis(
    categories = casos_15_59$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 2
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "Número de casos")
  ) %>%
  
  hc_add_series(
    data = casos_15_59$CASOS,
    name = "IRAG",
    color = "#252C61"
  )%>% 
  
  hc_legend(enabled = FALSE)


curva_interactiva_15_59


# 8- GRUPO ETARIO 60 AÑOS Y MAS - IRAG extendida --------------------------

grupo_60_mas <- distribucion_grupo_etario %>%
  filter(
    grupo_etario == "60 años y más",
    CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", "IRAG extendida"
  ))

casos_60_mas <- grupo_60_mas %>%
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(
    CASOS = n(),
    .groups = "drop"
  ) %>%
  arrange(SEPI)

#9 Paso datos a formato ancho (wider) para hacer curva interactiva

casos_60_mas <- casos_60_mas %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# 9- CURVA INTERACTIVA - IRAG EXTENDIDA EN PERSONAS DE 60 AÑOS Y MÁS ---------

curva_interactiva_60_mas <- highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(
    text = "Distribución semanal de casos de IRAG e IRAG extendida en personas de 
    60 años y más."
  ) %>%
  
  hc_subtitle(
    text = "Casos notificados según semana epidemiológica. 
    Unidad Centinela HRRG, 2024–2026."
  ) %>%
  
  hc_plotOptions(
    column = list(
      pointPadding = 0.1,
      groupPadding = 0.05,
      borderWidth = 0
    )
  ) %>%
  
  hc_xAxis(
    categories = casos_60_mas$SEPI,
    title = list(text = NULL),
    labels = list(
      rotation = -45,
      step = 2
    )
  ) %>%
  
  hc_yAxis(
    title = list(text = "Número de casos")
  ) %>%
  
  hc_add_series(
    data = casos_60_mas$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6"
  ) %>%
  hc_add_series(
    data = casos_60_mas$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61"
  ) %>%
   hc_legend(enabled = FALSE)

curva_interactiva_60_mas

