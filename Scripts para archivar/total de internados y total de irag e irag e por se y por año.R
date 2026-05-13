# Curva epidemiológica mostrando total de internados y total de IRAG e IRAGe por año y por SE.
# Año 2025

curva_epidemiologica_2025 <- data %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(n= n()) %>%
  ungroup() %>%
  complete(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION = 1:53, fill = list(n = 0)) %>%
  mutate(SE = str_pad(SEPI_MIN_INTERNACION, width = 2, side = "left", pad = "0"))

datos_unidos <- inner_join(curva_epidemiologica_2025, Totales_2025, by = "SE")



library (tidyr)  

datos_largo <- pivot_longer(datos_unidos, cols = c(n, Total),
                            names_to = "variable",
                            values_to =  "valor")
datos_largo$variable<- recode(datos_largo$variable, "n" = "IRAG/IRAGe", "Total" = "Total de internados")

ggplot(datos_largo, aes(x = SE,  y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Internados HRRG y Casos de IRAG e IRAGe 2025",
       x= "Semana epidemiológica -2025",
       y = "Casos",
       caption = "Fuente SNVS 2.0 y Agrupado UC IRAG RIO GALLEGOS") +
  theme_minimal()

# Año 2024

curva_epidemiologica_2024 <- data2 %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(n= n()) %>%
  ungroup() %>%
  complete(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION = 1:53, fill = list(n = 0)) %>%
  mutate(SE = str_pad(SEPI_MIN_INTERNACION, width = 2, side = "left", pad = "0"))

datos_unidos_2024 <- inner_join(curva_epidemiologica_2024, Totales_2024, by = "SE")

library (tidyr)  

datos_largo2 <- pivot_longer(datos_unidos_2024, cols = c(n, Total),
                            names_to = "variable",
                            values_to =  "valor")
datos_largo2$variable<- recode(datos_largo$variable, "n" = "IRAG/IRAGe", "Total" = "Total de internados")

ggplot(datos_largo2, aes(x = SE,  y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Internados HRRG y Casos de IRAG e IRAGe 2024",
       x= "Semana epidemiológica -2024",
       y = "Casos",
       caption = "Fuente SNVS 2.0 y Agrupado UC IRAG RIO GALLEGOS") +
  theme_minimal()




 