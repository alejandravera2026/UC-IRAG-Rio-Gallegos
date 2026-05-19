# ============================================
# Objetivo 3 - Porcentaje de positividad
# SARS - Cov2, Influenza y VSR
#=============================================

unique(data$INFLUENZA_FINAL)


#===========================================================================
# Se arma  gráfico de positividad por SE y tipo de virus
#===========================================================================

#Se arma otra base y se selecciona las  variables a estudiar

positividad_virus <- data %>% 
  select(SEPI,VSR_FINAL,INFLUENZA_FINAL,COVID_19_FINAL)


# Pasamos a formato largo (pivot longer)

positividad_virus <- positividad_virus %>%
  pivot_longer(cols = c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Agente", values_to = "resultado") %>%
  filter(resultado != "Sin resultado") %>%
  mutate(Agente = case_when(
    Agente == "INFLUENZA_FINAL" ~ "Influenza",
    Agente == "COVID_19_FINAL"  ~ "SARS-CoV-2",
    Agente == "VSR_FINAL"       ~ "VSR"
  ))


# Calculamos positividad por virus y semana

positividad_virus <- positividad_virus %>%
  group_by(SEPI, Agente) %>%
  summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(POSITIVIDAD = round(POSITIVOS/ESTUDIADOS *100,1))


#Se pasa a formato ancho (pivot wider) para gráfico interactivo

positividad_virus <- positividad_virus %>% pivot_wider(names_from = Agente,
                                                 values_from = POSITIVIDAD,
                                                 values_fill = 0)


#Grafico de líneas interactivo

positividad_lineas <- highchart() %>%
  hc_chart(type= "line") %>%
  hc_xAxis(title = list(text = "SE - Año"),
           categories = positividad_virus$SEPI) %>%
  hc_yAxis(
    title = list(text = "Porcentaje de positividad"),
    min = 0,
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


positividad_lineas


#========================================================================
# Se determina el total de virus positivos y por cada virus
#=========================================================================

total_positivos <- positividad_virus %>%
  summarise(total = sum(POSITIVOS, na.rm = TRUE)) %>%
  pull(total)
total_positivos  


positivos_influenza <- positividad_virus %>%
  filter(Agente == "Influenza") %>%
  summarise(n= sum(POSITIVOS, na.rm = TRUE)) %>%
  pull (n)

positivos_influenza

pct_influenza = (positivos_influenza/total_positivos)*100.1

pct_influenza

positivos_sars_cov <- positividad_virus %>%
  filter(Agente == "SARS-CoV-2") %>%
  summarise (n = sum(POSITIVOS, na.rm = TRUE)) %>%
  pull (n)

positivos_sars_cov

pct_sars_cov = (positivos_sars_cov/total_positivos)*100.1

pct_sars_cov

positivos_vsr <- positividad_virus %>%
  filter(Agente == "VSR") %>%
  summarise(n = sum(POSITIVOS, na.rm = TRUE)) %>%
  pull(n)

positivos_vsr

pct_vsr = (positivos_vsr/total_positivos)*100.1

pct_vsr
