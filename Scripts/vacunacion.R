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

# Se crea un vector de menores de 6 meses vacunacion materna

menores_seis_meses <-c("0 a 2 Meses", "3 a 5 Meses")

#Primero aplico a la vacunación materna antigripal

vacunacion_materna <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
  filter(VAC_ANTIGRIPAL_MATERNA!= "SIN DATO") %>%
  mutate(vaC_materna = case_when(VAC_ANTIGRIPAL_MATERNA %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADA",
                                 VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(vaC_materna)) %>%
  count(EDAD_UC_IRAG, vaC_materna) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Total = sum(n),
         Porcentaje = round(n/Total * 100,1)) %>%
  ungroup()

print(vacunacion_materna)

# Ahora aplico a la vacunación materna por VSR

unique(vacunacion$VAC_VSR)

vacunacion_materna_VSR <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
  filter(VAC_VSR!= "SIN DATO") %>%
  mutate(vaC_materna_VSR= case_when(VAC_VSR %in% c("SE 34", "SE 36", "SE 35", "SE 32", "SE DESCONOCIDA") ~ "VACUNADA",
                                 VAC_VSR == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(vaC_materna_VSR)) %>%
  count(EDAD_UC_IRAG, vaC_materna_VSR) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Total = sum(n),
         Porcentaje = round(n/Total * 100,1)) %>%
  ungroup()

print(vacunacion_materna_VSR)

# ESTUDIO LOS NIÑOS VACUNADOS DE 6 A 23 MESES Y ADULTOS MAYORES DE 60 AÑOS

unique(data$VAC_ANTIGRIPAL)
unique(data$EDAD_UC_IRAG)

#aplico vector de grupos de edad

entre_seis_y_23_meses <- c("6 a 11 Meses", "12 a 23 Meses")

mayoreS_60_años <- c("60 a 64 Años", "65 a 69 Años", "70 a 74 Años", "75 y más Años")

# Se estudia los vacunados por dos grupos de edad

# Primero entre 6 a 23 meses

vacunacion_6_23_meses <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% entre_seis_y_23_meses) %>%
  filter(VAC_ANTIGRIPAL!= "SIN DATO") %>%
  mutate(vaC_antigripal= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                    VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                    TRUE ~ NA_character_)) %>%
  filter(!is.na(vaC_antigripal)) %>%
  count(EDAD_UC_IRAG, vaC_antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Total = sum(n),
         Porcentaje = round(n/Total * 100,1)) %>%
  ungroup()

print(vacunacion_6_23_meses)vacunacion_6_23_meses <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% entre_seis_y_23_meses) %>%
  filter(VAC_ANTIGRIPAL!= "SIN DATO") %>%
  mutate(vaC_antigripal= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                   VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                   TRUE ~ NA_character_)) %>%
  filter(!is.na(vaC_antigripal)) %>%
  count(EDAD_UC_IRAG, vaC_antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Total = sum(n),
         Porcentaje = round(n/Total * 100,1)) %>%
  ungroup()

print(vacunacion_6_23_meses)

# Mayores de 60

vacunacion_mayores_60 <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% mayoreS_60_años) %>%
  filter(VAC_ANTIGRIPAL!= "SIN DATO") %>%
  mutate(vaC_antigripal= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                   VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                   TRUE ~ NA_character_)) %>%
  filter(!is.na(vaC_antigripal)) %>%
  count(EDAD_UC_IRAG, vaC_antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Total = sum(n),
         Porcentaje = round(n/Total * 100,1)) %>%
  ungroup()

print(vacunacion_mayores_60)










