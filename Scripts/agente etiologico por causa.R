#=============================================================================
# Objetivo 10: se define el agente etiológico en cuidados intensivos y en fallecidos
#============================================================================

#Se crea una base para el análisis

agente_etiologico_causa <- data %>%
  select(SEPI_MIN_INTERNACION, ANIO_MIN_INTERNACION,EDAD_DIAGNOSTICO, INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL,
         CUIDADO_INTENSIVO, FALLECIDO)



# Se completa y crea la variable SEPI

agente_etiologico_causa <- agente_etiologico_causa %>%
  complete(ANIO_MIN_INTERNACION,
        SEPI_MIN_INTERNACION = 1:52,
        fill = list(n = 0)) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-",str_pad(SEPI_MIN_INTERNACION,2,pad= "0")))


# Se define periodo de estudio

agente_etiologico_causa <- agente_etiologico_causa %>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & SEPI_MIN_INTERNACION <= SEMANA_MAXIMA)))

# Se crea los grupos de edad para el análisis

agente_etiologico_causa <- agente_etiologico_causa %>%
  mutate(grupo_etario_resumen = case_when(
    EDAD_DIAGNOSTICO >= 0 & EDAD_DIAGNOSTICO < 2 ~ "Menor de 2 años",
    EDAD_DIAGNOSTICO >= 2 & EDAD_DIAGNOSTICO < 15 ~ "2 a 14 años",
    EDAD_DIAGNOSTICO >= 15 & EDAD_DIAGNOSTICO < 65 ~ "15 a 64 años",
    EDAD_DIAGNOSTICO >= 65 ~ "Mayor de 65 años",
    TRUE ~ NA_character_))%>%
  mutate(grupo_etario_resumen = factor(grupo_etario_resumen, levels = c(
    "Menor de 2 años", "2 a 14 años", "15 a 64 años", "Mayor de 65 años")))

# Se pasa a formato pivot longer


agente_etiologico_causa <- agente_etiologico_causa %>%
  pivot_longer(cols = c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Agente", values_to = "resultado") %>%
  filter(resultado != "Sin resultado") %>%
  mutate(Agente = case_when(
    Agente == "INFLUENZA_FINAL" ~ "Influenza",
    Agente == "COVID_19_FINAL"  ~ "SARS-CoV-2",
    Agente == "VSR_FINAL"       ~ "VSR"
  ))


# Se hace el análisis de agente etiologico a cuidados intensivos

agente_etiologico_cuidados <- agente_etiologico_causa %>%
  group_by(grupo_etario_resumen, Agente) %>%
  summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(POSITIVIDAD = round(POSITIVOS/ESTUDIADOS *100,1))

#Se pasa a formato ancho (pivot wider) para gráfico interactivo

agente_etiologico_cuidados <- agente_etiologico_cuidados %>% pivot_wider(names_from = Agente,
                                                       values_from = POSITIVIDAD,
                                                       values_fill = 0)


#Grafico de líneas interactivo

positividad_cuidados <- highchart() %>%
  hc_chart(type= "bar") %>%
  hc_title(text = "Distribución de agentes etiológicos por grupos de edad en cuidados intensivos")
  hc_xAxis(
    categories = levels(agente_etiologico_cuidados$grupo_etario_resumen),
    title = list(text = "Grupo de edad")) %>%
    hc_add_yAxis(
      title = text = "Porcentaje"),
    max = 100,
    tickInterval = 10,
    labels = list(format = "{value}%")
  )  %>%
  hc_add_series(name = "Influenza", 
                data = positividad_virus$Influenza,
                color = "#f7941e") %>%
  hc_add_series(name = "VSR", 
                data = positividad_virus$VSR,
                color = "#00a651" ) %>%
  hc_add_series(name = "SARS-CoV-2",
                data = positividad_virus$`SARS-CoV-2`,
                color = "#C62828" )
  hc_yAxis(title = list(text= "Grupos de edad"), 
           categories = agente_etiologico_cuidados$grupo_etario_resumen)


positividad_lineas  