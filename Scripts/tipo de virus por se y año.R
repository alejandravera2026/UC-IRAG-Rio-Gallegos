# Creo una tabla con los virus 

tipo_virus <- data %>%
  select (ANIO_MIN_INTERNACION, SEPI_MIN_INTERNACION, INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL)

# Filtro los sin resultados

tipo_virus <- tipo_virus %>%
     filter(INFLUENZA_FINAL != "Sin resultado", COVID_19_FINAL != "Sin resultado", VSR_FINAL != "Sinr resultado")
 # Selecciono año y semana de estudio

ANIO_MINIMO <- 2024
SEMANA_MINIMA <- 18
ANIO_MAXIMO <- 2026
SEMANA_MAXIMA <- 8
  
# Uno semana con año  

tipo_virus <- tipo_virus %>%
     mutate(SEPI = paste(ANIO_MIN_INTERNACION, "-", SEPI_MIN_INTERNACION))

# Paso a formato largo

tipo_virus <- tipo_virus %>%
  pivot_longer(cols = c (INFLUENZA_FINAL, COVID_19_FINAL, VSR_FINAL),
               names_to = "Virus",
               values_to = "Resultado")

# Ver los tipos de virus por SE y por año

tipo_virus <- tipo_virus %>%
ggplot(tipo_virus, aes(x = SEPI, y = Resultado, fill = Resultado)) +
  geom_col() +
  facet_grid(Virus ~ SEPI, scales = "free_y") +
  scale_x_discrete(breaks = function(x) x[seq(1,length(x), by = 4)]) +
  scale_fill_brewer(palette = "Set1") +
  labs(title= "Casos por tipo de virus por Se y año",
       caption = "Fuente SNVS 2.0",
       x= "Semana epidemiológica -año",
       y= "N° de casos",
       fill = "Resultado") +
  theme_minimal() +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
  
    

               
