#==============================================================================
#Objetivo 2: Describir la distribución de casos de IRAG e IRAGE por grupo etario,
#por semana epidemiológica y  por año, identificando 
#grupos poblaciones de mayor riesgo. 
#===============================================================================


#Selecciono las variables para analizar 

distribucion_grupo_etario <- data %>%
  select(CLASIFICACION_MANUAL, grupo_etario, SEPI)

#=====================================================================
# Grupo etario menor de 2 años
#====================================================================

unique(distribucion_grupo_etario$grupo_etario)

menor_dos_años <- distribucion_grupo_etario %>%
  filter(grupo_etario %in% "< 2 años")

casos_menores_dos_años <- menor_dos_años %>% 
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_menores_dos_años <- casos_menores_dos_años %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva_menores_dos <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de IRAG e IRAG extendida por semana y año. Menores de 2 años")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = casos_menores_dos_años$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_menores_dos_años$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_menores_dos_años$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva_menores_dos

#===========================================================================
#Grupo etario de 2 a 14 años
#===========================================================================

dos_a_14_años <- distribucion_grupo_etario %>%
  filter(grupo_etario %in% "2-14 años")

casos_dos_a_14_años <- dos_a_14_años %>% 
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_dos_a_14_años <- casos_dos_a_14_años %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva_dos_a_14_años <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de IRAG e IRAG extendida por semana y año. De 2 a 14 años")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = dos_a_14_años$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_dos_a_14_años$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_dos_a_14_años$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva_dos_a_14_años

#=============================================================================
# Grupo etario de 15 a 59 años
#=============================================================================

de_15_a_59_años <- distribucion_grupo_etario %>%
  filter(grupo_etario %in% "15-59 años")

casos_de_15_a_59_años <- de_15_a_59_años %>% 
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_de_15_a_59_años <- casos_de_15_a_59_años %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva_de_15_a_59 <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de IRAG e IRAG extendida por semana y año. De 15 a 59 años")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = de_15_a_59_años$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_de_15_a_59_años$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_de_15_a_59_años$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva_de_15_a_59

#========================================================================
#Grupos mayores de 60 años
#========================================================================

mayores_60_años <- distribucion_grupo_etario %>%
  filter(grupo_etario %in% "60 años y más")

casos_mayores_60_años <- mayores_60_años %>% 
  group_by(SEPI,CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n()) %>%
  ungroup() %>%
  arrange(SEPI) 

# Paso datos a formato ancho (wider) para hacer curva interactiva

casos_mayores_60_años <- casos_mayores_60_años %>% 
  pivot_wider(names_from = CLASIFICACION_MANUAL,
              values_from = CASOS,
              values_fill = 0) 


# CURVA INTERACTIVA CASOS DE IRAG E IRAGE ---------------------------------

curva_interactiva_mayores_60 <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(
    text = "Distribución temporal de IRAG e IRAG extendida por semana y año. Mayores de 60 años")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = mayores_60_años$SEPI, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_add_series(
    data = casos_mayores_60_años$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#252C61") %>%
  hc_add_series(
    data = casos_mayores_60_años$`IRAG extendida`,
    name = "IRAG extendida",
    color = "#7EC8E6")

curva_interactiva_mayores_60

#=============================================================================
# Se determina los números de casos de IRAG e IRAG extendida en menores de dos
#años y mayores de 60. Además, se realizan los cálculos en porcentaje
#=============================================================================

###Menores de dos años

notificaciones_totales1 <- menor_dos_años%>%
  filter(CLASIFICACION_MANUAL %in%c("Infección respiratoria aguda grave (IRAG)",
                                    "IRAG extendida"))%>%
  summarise(Total = n())

notificaciones_totales1

notificaciones_irag1 <- menor_dos_años %>%
  filter(CLASIFICACION_MANUAL %in% "Infección respiratoria aguda grave (IRAG)") %>%
  summarise(IRAG = n())

notificaciones_irag1

notificaciones_irage1 <- menor_dos_años %>%
  filter(CLASIFICACION_MANUAL %in% "IRAG extendida") %>%
  summarise(IRAGE = n())

notificaciones_irage1

pct_IRAG1 <- menor_dos_años %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", 
                                     "IRAG extendida")) %>%
  summarise(
    n_IRAG = sum(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)"),
    n_Total = n())%>%
  mutate(pct = round(n_IRAG/n_Total *100.1))%>%
  pull(pct)

pct_IRAG1

pct_IRAGE1 <- menor_dos_años %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)",
                                     "IRAG extendida")) %>%
  summarise(
    n_IRAGE = sum (CLASIFICACION_MANUAL == "IRAG extendida"),
    n_Total = n()) %>%
  mutate(pct = round(n_IRAGE/n_Total *100.1)) %>%
  pull (pct)

pct_IRAGE1

####Mayores de 60 años

notificaciones_totales2 <- mayores_60_años %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)",
                                    "IRAG extendida"))%>%
  summarise(Total = n())

notificaciones_totales2

notificaciones_irag2 <- mayores_60_años %>%
  filter(CLASIFICACION_MANUAL %in% "Infección respiratoria aguda grave (IRAG)") %>%
  summarise(IRAG = n())

notificaciones_irag2

notificaciones_irage2 <- mayores_60_años %>%
  filter(CLASIFICACION_MANUAL %in% "IRAG extendida") %>%
  summarise(IRAGE = n())

notificaciones_irage2

pct_IRAG2 <- mayores_60_años %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)", 
                                     "IRAG extendida")) %>%
  summarise(
    n_IRAG = sum(CLASIFICACION_MANUAL == "Infección respiratoria aguda grave (IRAG)"),
    n_Total = n())%>%
  mutate(pct = round(n_IRAG/n_Total *100.1))%>%
  pull(pct)

pct_IRAG2

pct_IRAGE2 <- mayores_60_años %>%
  filter(CLASIFICACION_MANUAL %in% c("Infección respiratoria aguda grave (IRAG)",
                                     "IRAG extendida")) %>%
  summarise(
    n_IRAGE = sum (CLASIFICACION_MANUAL == "IRAG extendida"),
    n_Total = n()) %>%
  mutate(pct = round(n_IRAGE/n_Total *100.1)) %>%
  pull (pct)

pct_IRAGE2

