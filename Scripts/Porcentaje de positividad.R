# Determinaciones positivas por se y por año

ANIO_MINIMO <- 2024
ANIO_MAXIMO <-2026
SEMANA_MINMIA <-18
SEMANA_MAXIMA <-8

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
  filter(DETERMINACIONES >=5)

ggplot(positividad_se, aes(x = SEPI_MIN_INTERNACION, y= PORCENTAJE_POSITIVIDAD, color = factor(ANIO_MIN_INTERNACION) )) +
  geom_line(linewidth = 1) +
  geom_point(size= 2) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0, 100, 10),
                     labels = scales::label_percent(scale = 1)) +
  scale_x_continuous(breaks = seq (0, 52, 4)) +
  labs(title ="Porcentaje de positividad por SE y por año",
       caption = "Fuente SNVS 2.0",
       X = "Semana epidemiológica",
       y = "% Positividad",
       color = "Año") +
  theme_minimal() +
  theme(legend.position = "bottom")