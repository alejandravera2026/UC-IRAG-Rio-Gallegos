
# Objetivo 7: estimar la proporción de irag e irag e que requieren ingreso a cuidados intensivos

#Procesamiento de base individual

str(data)

colnames(data)#Busco mis variables

unique (data$CUIDADO_INTENSIVO)

unique(data$CLASIFICACION_MANUAL)

data <- data %>% select(CLASIFICACION_MANUAL,CUIDADO_INTENSIVO,FECHA_CUI_INTENSIVOS,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION)

irag_uci <- data %>% 
  filter(CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)")

irag_uci <- irag_uci %>% # Resumo 
  group_by(`CUIDADO_INTENSIVO`) %>%  
  summarise(Casos_irag = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

print(irag_uci)

#====================================================================================================
uci_si <- data %>% filter(CUIDADO_INTENSIVO=="SI")

uci_si <- uci_si %>% # Resumo crónicas por gedad
  group_by(`CLASIFICACION_MANUAL`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

print(uci_si)
