# Se define una tabla para usando en el nominal edad < 2 y > 60 y embarazadas y vacunas

colnames(data)
unique(data$EDAD_UC_IRAG)

#FILTRO SEGÚN PERÍODO DE ESTUDIO

data <- data%>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO_MIN_INTERNACION > ANIO_MINIMO | 
       (ANIO_MIN_INTERNACION == ANIO_MINIMO & SEPI_MIN_INTERNACION >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO_MIN_INTERNACION< ANIO_MAXIMO | 
         (ANIO_MIN_INTERNACION == ANIO_MAXIMO & SEPI_MIN_INTERNACION <= SEMANA_MAXIMA))
  )

unique(data$EDAD_UC_IRAG)

# Selecciono columnas a estudiar

vacunacion <- data %>%
  select(EDAD_UC_IRAG, VAC_ANTIGRIPAL, VAC_ANTIGRIPAL_MATERNA, VAC_VSR, CLASIFICACION_MANUAL)

unique(vacunacion$EDAD_UC_IRAG)

menores_seis_meses <- c("0 a 2 Meses", "3 a 5 Meses")


#Primero aplico a la vacunación materna antigripal

vacunacion_materna <- vacunacion %>%
  filter(VAC_ANTIGRIPAL_MATERNA!= "SIN DATO") %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
  mutate(Vac_Materna_Antigripal = case_when(VAC_ANTIGRIPAL_MATERNA %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADA",
                                 VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(Vac_Materna_Antigripal)) %>%
  count(EDAD_UC_IRAG, Vac_Materna_Antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje= n/sum(n)) %>%
  gt () %>%
  fmt_percent(columns = porcentaje, decimals = 1)

vacunacion_materna

# Ahora aplico a la vacunación materna por VSR

unique(vacunacion$VAC_VSR)
unique(vacunacion$EDAD_UC_IRAG)

vacunacion_materna_vsr <- vacunacion %>%
  filter(VAC_VSR!= "SIN DATO") %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses)%>%
  mutate(vacuna_materna_sincicial = case_when(VAC_VSR %in% c("SE 36", "SE 35", "SE 32",
                                                            "SE 34", "SE DESCONOCIDA") ~
                                                "MADRE VACUNADA",
                                              VAC_VSR == "MADRE NO VACUNADA" ~ "NO VACUNADA VSR",
         TRUE ~ NA_character_)) %>%
  mutate(menor_seis = case_when(EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses")~ "Menor de seis meses",
                                TRUE ~ NA_character_))%>%
  filter(!is.na(vacuna_materna_sincicial)) %>%
  count(EDAD_UC_IRAG, vacuna_materna_sincicial) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/sum(n)) %>%
  ungroup() %>%
  gt () %>%
  fmt_percent(porcentaje, decimals = 1)
  
  vacunacion_materna_vsr
  
# ESTUDIO LOS NIÑOS VACUNADOS DE 6 A 23 MESES Y ADULTOS MAYORES DE 60 AÑOd
unique(data$VAC_ANTIGRIPAL)
unique(data$EDAD_UC_IRAG)

#aplico vector de grupos de edad

entre_seis_y_23_meses <- c("6 a 11 Meses", "12 a 23 Meses")

mayores_65_años <- c("65 a 69 Años", "70 a 74 Años", "75 y más Años")

# Se estudia los vacunados por dos grupos de edad

# Primero entre 6 a 23 meses

vacunacion_6_23_meses <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% entre_seis_y_23_meses) %>%
  filter(VAC_ANTIGRIPAL!= "SIN DATO") %>%
  mutate(vac_antigripal_niños= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                    VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                    TRUE ~ NA_character_)) %>%
  filter(!is.na(vac_antigripal_niños)) %>%
  count(EDAD_UC_IRAG, vac_antigripal_niños) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  gt () %>%
  fmt_percent(porcentaje, decimals = 1)

vacunacion_6_23_meses
  

# Mayores de 65 años

vacunacion_mayores_65 <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% mayoreS_65_años) %>%
  filter(VAC_ANTIGRIPAL!= "SIN DATO") %>%
  mutate(vac_antigripal_adultos= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                   VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                   TRUE ~ NA_character_)) %>%
  filter(!is.na(vac_antigripal_adultos)) %>%
  count(EDAD_UC_IRAG, vac_antigripal_adultos) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  gt () %>%
  fmt_percent(porcentaje, decimals = 1)


vacunacion_mayores_65

## Representación gráfica de vacunación

## Antigripal Materna

vacunacion_materna_grafico <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
  filter(VAC_ANTIGRIPAL_MATERNA != "SIN DATO") %>%
  mutate(Vac_Materna_Antigripal = case_when(VAC_ANTIGRIPAL_MATERNA %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADA",
                                            VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                            TRUE ~ NA_character_)) %>%
  filter(!is.na(Vac_Materna_Antigripal)) %>%
  count(EDAD_UC_IRAG, Vac_Materna_Antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/ sum(n)) %>%
  ungroup()

  ggplot(vacunacion_materna_grafico, aes( x = EDAD_UC_IRAG, y = porcentaje, fill = Vac_Materna_Antigripal)) +
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje, 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("VACUNADA" = "#1E88E5",
                               "NO VACUNADA" = "#FB8C00")) +
    labs(title = "Cobertura de vacunación materna antigripal", 
         x = "",
         y = "%",
         caption = "Fuente SNVS 2.0") +
    theme_minimal()
  
## Vacunación materna VSR
  vacunacion_materna_vsr_grafico <- vacunacion %>%
    filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
    filter(VAC_VSR!= "SIN DATO") %>%
    mutate(Vac_Materna_VSR = case_when(VAC_VSR %in% c("SE 36", "SE 35", "SE 32",
                                                      "SE 34", "SE DESCONOCIDA") ~ "VACUNADA",
                                              VAC_VSR == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                              TRUE ~ NA_character_)) %>%
    filter(!is.na(Vac_Materna_VSR)) %>%
    count(EDAD_UC_IRAG, Vac_Materna_VSR) %>%
    group_by(EDAD_UC_IRAG) %>%
    mutate(porcentaje = n/ sum(n)) %>%
    ungroup()
  
  ggplot(vacunacion_materna_vsr_grafico, aes( x = EDAD_UC_IRAG, y = porcentaje, fill = Vac_Materna_VSR)) +
    geom_col() +
    geom_text(aes(label = scales::percent(porcentaje, 0.1)),
              position = position_stack(vjust = 0.5),
              color = "white", size = 4, fontface = "bold") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(values = c("VACUNADA" = "#1E88E5",
                                 "NO VACUNADA" = "#FB8C00")) +
    labs(title = "Cobertura de vacunación materna VSR", 
         x = "",
         y = "%",
         caption = "Fuente SNVS 2.0") +
    theme_minimal()

## Vacunación antigripal de 6 meses a 23 meses y mayores de 65 años
  
unique(vacunacion$VAC_ANTIGRIPAL)

vacunacion_antigripal_grafico <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% entre_seis_y_23_meses) %>%
  filter(VAC_ANTIGRIPAL != "SIN DATO") %>%
  mutate(vacunacion_antigripal = case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA")~ "VACUNA ANTIGRIPAL",
                                           VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO ANTIGRIPAL",
                                           TRUE ~NA_character_)) %>%
  filter(!is.na(vacunacion_antigripal)) %>%
  count(EDAD_UC_IRAG,vacunacion_antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/sum(n))%>%
  ungroup()

ggplot(vacunacion_antigripal_grafico, aes( x = EDAD_UC_IRAG, y = porcentaje, fill = vacunacion_antigripal)) +
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje, 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("VACUNA ANTIGRIPAL" = "#1E88E5",
                               "NO VACUNADO ANTIGRIPAL" = "#FB8C00")) +
  labs(title = "Cobertura de vacunación antigripal en niños de sies meses a 23 meses internados por IRAG e IRAG extendida", 
       x = "",
       y = "%",
       caption = "Fuente SNVS 2.0") +
  theme_minimal()

vacunacion_antigripal_grafico_mayores <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% mayores_65_años) %>%
  filter(VAC_ANTIGRIPAL != "SIN DATO") %>%
  mutate(vacunacion_antigripal = case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA")~ "VACUNA ANTIGRIPAL",
                                           VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO ANTIGRIPAL",
                                           TRUE ~NA_character_)) %>%
  filter(!is.na(vacunacion_antigripal)) %>%
  count(EDAD_UC_IRAG,vacunacion_antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(porcentaje = n/sum(n))%>%
  ungroup()

ggplot(vacunacion_antigripal_grafico_mayores, aes( x = EDAD_UC_IRAG, y = porcentaje, fill = vacunacion_antigripal)) +
  geom_col() +
  geom_text(aes(label = scales::percent(porcentaje, 0.1)),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("VACUNA ANTIGRIPAL" = "#1E88E5",
                               "NO VACUNADO ANTIGRIPAL" = "#FB8C00")) +
  labs(title = "Cobertura de vacunación antigripal mayores de 65 años internados por IRAG e IRAG extendida", 
       x = "",
       y = "%",
       caption = "Fuente SNVS 2.0") +
  theme_minimal()



  
  
  







