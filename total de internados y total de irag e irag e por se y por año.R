# Curva epidemiológica mostrando total de internados y total de IRAG e IRAGe por año y por SE.

curva_epidemiologica_2025 <- data1 %>%
  group_by(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION) %>%
  summarise(n= n()) %>%
  ungroup() %>%
  complete(ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION = 1:53, fill = list(n = 0)) %>%
  mutate(SE = str_pad(SEPI_MIN_INTERNACION, width = 2, side = "left", pad = "0"))

datos_unidos <- inner_join(curva_epidemiologica_2025, Totales_2025, by = "SE")

ggplot(datos_unidos, aes(x = SE)) +
  geom_line(aes(y = Total, color = "Internados")) +
  geom_point(aes(y = Total, color = "Internados")) +
  geom_line(aes(y = n , color = "IRAG e IRAGe")) +
  geom_point(aes(y = n, color = "IRAG e IRAGe")) +
  labs(title = "Curva epidemiologica total de  internados y total de irag e irage-2025",
       x = "Semana epidemiológica",
       y = "Casos",
       caption = "Fuente SNVS 2.0 y Agrupado Unidad Centinela") +
  scale_color_manual(values = c("Internados" = "blue", "IRAG e IRAe"= "red")) +
  theme_minimal()

library (tidyr)  

datos_largo <- pivot_longer(datos_unidos, cols = c(n, Total),
                            names_to = "variable",
                            values_to =  "valor")
datos_largo$variable<- recode(datos_largo$variable, "n" = "IRAG/IRAGe", "Total" = "Total de interados")

ggplot(datos_largo, aes(x = SE,  y = valor, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total de Internados HRRG y Casos de IRAG e IRAGe 2025",
       x= "Semana epidemiológica -2025",
       y = "Casos",
       caption = "Fuente SNVS 2.0 y Agrupado UC IRAG RIO GALLEGOS") +
  theme_minimal()
 