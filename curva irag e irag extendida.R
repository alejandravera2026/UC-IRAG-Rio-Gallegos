

#  curva epidemiológica por SE-2025 por IRAG e IRAG extendida


curva_epidemiologica_casos_2025 <- data1 %>% group_by(CLASIFICACION_MANUAL, SEPI_MIN_INTERNACION) %>%
  summarise(n = n()) %>%
  ungroup()

curva_epidemiologica_casos_2025<- curva_epidemiologica_casos_2025 %>% mutate(SE = str_pad(SEPI_MIN_INTERNACION,
                                      width = 2, 
                                      side = "left", 
                                      pad = "0"))

curva_epidemiologica_casos_2025 <- curva_epidemiologica_casos_2025 %>% 
  complete(SEPI_MIN_INTERNACION = 1:53,
           fill = list (n= 0))


colores <- c("#7fc97f","#beaed4") 

grafico_curva_casos_2025 <- ggplot(curva_epidemiologica_casos_2025, aes(x = SE, y = n, fill = CLASIFICACION_MANUAL)) +
  geom_col () +
  labs (title = "Casos de IRAG e IRAG extendida por SE año 2025",
        x = "Semana epidemiológica",
        y = "Casos",
        caption = "Fuente: elaboración propia a partir de datos del SNVS 2.0.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  scale_fill_manual(values = colores,
                    name = "",
                    labels = c("Infección respiratoria aguda (IRAG)","IRAG extendida"),
                    na.translate = FALSE) +
  theme(legend.position = "bottom")

grafico_curva_casos_2025

# Curva epidemiológica Año 2024 - IRAG E IRAG EXTENDIDA

curva_epidemiologica_casos_2024 <- data2 %>% group_by(CLASIFICACION_MANUAL, SEPI_MIN_INTERNACION) %>%
  summarise(n = n()) %>%
  ungroup()


unique(curva_epidemiologica_casos_2024$CLASIFICACION_MANUAL)
table(curva_epidemiologica_casos_2024$CLASIFICACION_MANUAL)

curva_epidemiologica_casos_2024 <- curva_epidemiologica_casos_2024 %>%
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología")

curva_epidemiologica_casos_2024<- curva_epidemiologica_casos_2024 %>% mutate(SE = str_pad(SEPI_MIN_INTERNACION,
                                                                                          width = 2, 
                                                                                          side = "left", 
                                                                                          pad = "0"))
curva_epidemiologica_casos_2024 <- curva_epidemiologica_casos_2024 %>% 
  complete(CLASIFICACION_MANUAL, SEPI_MIN_INTERNACION = 1:53,
           fill = list (n= 0))

colores <- c("#7fc97f","#beaed4") 

grafico_curva_casos_2024 <- curva_epidemiologica_casos_2024 %>% 
  ggplot(aes(x = SE, y = n, fill = CLASIFICACION_MANUAL)) +
  geom_col() +
  labs (title = "Casos de IRAG e IRAG extendida por SE año 2024",
        x = "Semana epidemiológica",
        y = "Casos",
        caption = "Fuente: elaboración propia a partir de datos del SNVS 2.0.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5)) +
  scale_fill_manual(values = colores,
                    name = "",
                    labels = c("Infección respiratoria aguda grave (IRAG)","IRAG extendida"),
                    na.translate = FALSE) +
  theme(legend.position = "bottom")
grafico_curva_casos_2024









