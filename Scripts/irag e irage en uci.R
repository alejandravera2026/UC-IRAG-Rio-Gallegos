
# Objetivo 7: estimar la proporción de irag e irag e que requieren ingreso a cuidados intensivos

#Procesamiento de base individual

str(data)

colnames(data)#Busco mis variables

unique (data$CUIDADO_INTENSIVO)

unique (data$FECHA_CUI_INTENSIVOS)

unique(data$CLASIFICACION_MANUAL)

data <- data %>% 
  filter (CLASIFICACION_MANUAL!="Caso invalidado por epidemiología")

#=================================================================================
#==================================================================================
# IRAG =============================================================================
#======================================================================================

data <- data %>% select(CLASIFICACION_MANUAL,CUIDADO_INTENSIVO,FECHA_CUI_INTENSIVOS,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION)

irag_uci <- data %>% 
  filter(CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)")

irag_uci <- irag_uci %>% # Resumo 
  group_by(`CUIDADO_INTENSIVO`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

print(irag_uci)

#=================================================================================
#==================================================================================
# IRAGE =============================================================================
#======================================================================================

data <- data %>% select(CLASIFICACION_MANUAL,CUIDADO_INTENSIVO,FECHA_CUI_INTENSIVOS,SEPI_MIN_INTERNACION,ANIO_MIN_INTERNACION)

irage_uci <- data %>% 
  filter(CLASIFICACION_MANUAL=="IRAG extendida")

irage_uci <- irage_uci %>% # Resumo 
  group_by(`CUIDADO_INTENSIVO`) %>%  
  summarise(Casos = n(), .groups = "drop") %>% 
  mutate(
    Porcentaje = round((Casos / sum(Casos)) * 100,1)
  ) %>%
  arrange(desc(Porcentaje)) %>%   # primero ordena
  adorn_totals(where = "row")     # después agrega Total

print(irage_uci)

#====================================================================================================
