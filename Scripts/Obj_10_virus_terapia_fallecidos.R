#=============================================================================
# Objetivo 10: se define el agente etiológico en cuidados intensivos y en fallecidos
#============================================================================

#Se crea una base para el análisis

agente_etiologico_causa <- data %>%
  select(SEPI,EDAD_DIAGNOSTICO, INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL,
         CUIDADO_INTENSIVO, FALLECIDO)

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
  filter(CUIDADO_INTENSIVO =="SI")%>%
  summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(POSITIVIDAD = round(POSITIVOS/ESTUDIADOS *100,1))



#=======================================================================================
#Grafico 
#=======================================================================================




#===========================================================================
# se hace la misma gráfica para fallecidos
#===========================================================================
agente_etiologico_FALLECIDOS <- agente_etiologico_causa %>%
  group_by(grupo_etario_resumen, Agente) %>%
  filter(FALLECIDO == "SI") %>%
summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(POSITIVIDAD = round(POSITIVOS/ESTUDIADOS *100,1))

orden_grupos <- c("Menor de 2 años", "2 a 14 años", "15 a 64 años", "Mayor de 65 años")

datos_fallecidos <- agente_etiologico_FALLECIDOS %>%
  complete(grupo_etario_resumen = orden_grupos, Agente, fill = list (POSITIVIDAD = 0)) %>%
  mutate(grupo_etario_resumen = factor(grupo_etario_resumen, levels = orden_grupos, 
                                       ordered = TRUE)) %>%
  arrange(grupo_etario_resumen)
   



#=======================================================================================
#Grafico 
#=======================================================================================

highchart() %>%
  hc_chart(type = "bar", inverted = TRUE) %>%
  hc_title (text = "Positividad por agente etiológico  y grupo de edad en fallecidos") %>%
  hc_xAxis(
    categories = levels(datos_fallecidos$grupo_etario_resumen),
    title = list(text = " Grupo etario")) %>%
  hc_yAxis(
    title = list(text = "Positividad"),
    max = 50,
    labels = list(format= "{value}%")) %>%
  hc_add_series(
    name = "Influenza",
    data = datos_fallecidos%>%
      filter(Agente == "Influenza") %>% pull (POSITIVIDAD),
    color = "#f7941e") %>%
  hc_add_series(
    name = "SARS-CoV-2",
    data = datos_fallecidos %>%
      filter(Agente == "SARS-CoV-2") %>% pull (POSITIVIDAD),
    color = "#C62828") %>%
  hc_add_series(
    name = "VSR",
    data = datos_fallecidos %>%
      filter(Agente == "VSR") %>% pull (POSITIVIDAD),
    color = "#00a651") %>%
  hc_plotOptions(
    bar = list(
      dataLabels = list(enabled = TRUE, format = "{y}%"),
      groupPadding = 0.1,
      pointPadding = 0.05)) %>%
  hc_legend(
    title = list(text = "Agente etiológico"),
    align = "center",
    verticalAlign = "bottom"
  ) %>%
  hc_tooltip(shared = TRUE, valueSuffix = "%")

















    

