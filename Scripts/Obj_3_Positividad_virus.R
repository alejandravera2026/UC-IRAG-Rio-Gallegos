# ============================================
# Objetivo 3 - Porcentaje de positividad
# SARS - Cov2, Influenza y VSR
#=============================================
unique(data$INFLUENZA_FINAL)

positividad_se <- data %>%
  filter(INFLUENZA_FINAL != "Sin Resultado"| COVID_19_FINAL != "Sin resultado" | VSR_FINAL != "Sin resultado") %>%
  pivot_longer(cols =  c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Virus",
               values_to = "Resultado") %>%
  filter(Resultado != "Sin resultado") %>%
  mutate(POSITIVO = case_when(Virus == "INFLUENZA_FINAL"& Resultado != "Negativo"~1,
                              Virus %in% c("COVID_19_FINAL", "VSR_FINAL") & Resultado == "Positivo"~1,
                              TRUE ~ 0)) %>%
  group_by( ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(DETERMINACIONES = n (),
            POSITIVOS = sum(POSITIVO),
            PORCENTAJE_POSITIVIDAD = round(POSITIVOS/DETERMINACIONES * 100.1),
            .groups = "drop") %>%
  filter(DETERMINACIONES >=5) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-", SEPI_MIN_INTERNACION))

ggplot(positividad_se, aes(x = SEPI, y= PORCENTAJE_POSITIVIDAD, group = 1 )) +
  geom_line(linewidth = 1, color ="#2C5") +
  geom_point(size= 2, color = "#2C5") +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0, 100, 10),
                     labels = scales::label_percent(scale = 1)) +
  scale_x_discrete(breaks = function(x) x[seq(1,length(x), by = 4)]) +
  labs(title ="Porcentaje de positividad por SE y por año",
       caption = "Fuente SNVS 2.0",
       x = "Semana por semana y por año",
       y = "% Positividad") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Positividad por SE y virus

#Selecciono variables

positividad_se <- data %>% select(ANIO_MIN_INTERNACION,SEPI_MIN_INTERNACION,
                                  VSR_FINAL,INFLUENZA_FINAL,COVID_19_FINAL)


#Completo SE y creo variable SEPI
positividad_se <- positividad_se %>% complete(ANIO_MIN_INTERNACION,
                                              SEPI_MIN_INTERNACION = 1:52,
                                              fill = list(n = 0)) %>%
  mutate(SEPI= paste(ANIO_MIN_INTERNACION,"-",str_pad(SEPI_MIN_INTERNACION,2,pad= "0")))


#Filtro según periodo de estudio

positividad_se <- positividad_se %>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & SEPI_MIN_INTERNACION <= SEMANA_MAXIMA))
  )


# Pasamos a formato largo (pivot longer)

positividad_se <- positividad_se %>%
  pivot_longer(cols = c(INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Agente", values_to = "resultado") %>%
  filter(resultado != "Sin resultado") %>%
  mutate(Agente = case_when(
    Agente == "INFLUENZA_FINAL" ~ "Influenza",
    Agente == "COVID_19_FINAL"  ~ "SARS-CoV-2",
    Agente == "VSR_FINAL"       ~ "VSR"
  ))


# Calculamos positividad por virus y semana

positividad_se <- positividad_se %>%
  group_by(SEPI, Agente) %>%
  summarise(
    ESTUDIADOS = n(),
    POSITIVOS = sum(resultado != "Negativo"),
    .groups = "drop"
  ) %>%
  mutate(POSITIVIDAD = round(POSITIVOS/ESTUDIADOS *100,1))


#Pasamos a formato ancho (pivot wider) para gráfico interactivo
positividad_se <- positividad_se %>% pivot_wider(names_from = Agente,
                                                 values_from = POSITIVIDAD,
                                                 values_fill = 0)


#Grafico de líneas interactivo

positividad_lineas <- highchart() %>%
  hc_chart(type= "line") %>%
  hc_title(text="Porccentaje de positividad para SARS Cov 2, Influenza y VSR") %>%
  hc_xAxis(title = list(text = "SE - Año"),
           categories = positividad_se$SEPI) %>%
  hc_yAxis(
    title = list(text = "Porcentaje de positividad"),
    min = 0,
    max = 100,
    tickInterval = 10,
    labels = list(format = "{value}%")
  )  %>%
  hc_add_series(name = "Influenza", 
                data = positividad_se$Influenza,
                color = "#252C61") %>%
  hc_add_series(name = "VSR", 
                data = positividad_se$VSR,
                color = "#c44228" ) %>%
  hc_add_series(name = "SARS-Cov-2", 
                data = positividad_se$`SARS-CoV-2`,
                color = "#7a9E7E" )


positividad_lineas
