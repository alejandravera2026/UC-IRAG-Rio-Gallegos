
#===============================================================================
#Objetivo 5: Determinar la proporción de pacientes vacunados en los grupos de riesgo
#(6 a 23 meses y mayores de 65 años) y  en madres vacunadas durante el embarazo 
#(menores de 6 meses)
#===============================================================================


colnames(data)

#===============================================================================
# Selecciono columnas a estudiar
#===============================================================================

vacunacion <- data %>%
  select(EDAD_UC_IRAG, VAC_ANTIGRIPAL, VAC_ANTIGRIPAL_MATERNA,VAC_MATERNA_VSR, CLASIFICACION_MANUAL) 

#=============================================================================
# Armo un vector para agrupar los grupos de edad para estudiar la vacunación
#=============================================================================


unique(vacunacion$EDAD_UC_IRAG)

vacunacion <- vacunacion %>%
  mutate(grupo_edad = case_when(
    EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses")~ "Menores de 6 meses",
    EDAD_UC_IRAG %in% c("6 a 11 Meses", "12 a 23 Meses") ~ "6 a 23 meses",
    EDAD_UC_IRAG %in% c("65 a 69 Años", "70 a 74 Años", "75 y más Años") ~ "Mayores de 65 años",
    TRUE ~ "24 meses a 64 años"
  ))

#==============================================================================
#VACUNACION MATERNA PARA ANTIGRIPAL Y VSR
#==============================================================================

vacunacion_materna <- vacunacion %>%
  filter(grupo_edad == "Menores de 6 meses") %>%
  select(
    "VSR Materna" = VAC_MATERNA_VSR,
    "Antigripal Materna" = VAC_ANTIGRIPAL_MATERNA)%>%
  pivot_longer(
    cols = everything(),
    names_to = "Vacuna",
    values_to = "Estado") %>%
  count(Vacuna, Estado) %>%
  group_by(Vacuna) %>%
  mutate(prop = n/sum(n) *100) %>%
  ungroup() %>%
  
  ggplot(aes(x= Vacuna, y = prop, fill = Estado)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text (
    aes(label = ifelse(prop > 2, paste0(round(prop,1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white")+
  scale_fill_manual(values = c("VACUNADA" = "#A23B72", "NO VACUNADA" = "#2E86AB",
                               "SIN DATO" ="#8b5a2b"),
                    breaks = c("VACUNADA", "NO VACUNADA", "SIN DATO")) +
  labs (title = "Vacunación materna en menores de 6 meses",
        x = "",
        y = "%",
        fill = "Estado") +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")
  
vacunacion_materna

#===============================================================================
# VACUNACION ANTIGRIPAL EN DOS GRUPOS DE EDAD: 6 A 23 MESES Y MAYORES DE 65 AÑOS
#===============================================================================

vacunacion_antigripal <- vacunacion %>%
  filter(grupo_edad %in% c("6 a 23 meses", "Mayores de 65 años")) %>%
  select(grupo_edad, "Antigripal" = VAC_ANTIGRIPAL) %>%
  count(grupo_edad, Antigripal) %>%
  group_by(grupo_edad) %>%
  mutate(prop = n/sum (n)*100) %>%
  ungroup() %>%
  ggplot(aes(x = grupo_edad, y = prop, fill = Antigripal)) +
  geom_col(position = "stack", width = 0.6) +
  geom_text(
    aes(label = ifelse(prop >2, paste0(round(prop,1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 4,
    color = "white")+
  scale_fill_manual(
    values = c("VACUNADO" = "#A23B72", "NO VACUNADO" = "#2E86AB", 
               "SIN DATO" = "#8b5a2b"
    ),
    breaks = c("VACUNADO", "NO VACUNADO", "SIN DATO"))+
  labs(title = "Vacunación antigripal por grupos de edad",
       x = "",
       y = "%",
       fill ="Estado")+
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom")

vacunacion_antigripal    

  

         
         
         


  
  
  







