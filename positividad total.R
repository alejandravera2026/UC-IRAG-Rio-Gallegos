#=============================================================================
#positividad de virus por se
#=============================================================================



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
       caption = 