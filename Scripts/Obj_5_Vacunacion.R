#===============================================================================
#Objetivo 5: Determinar la proporción de pacientes vacunados en los grupos de riesgo
#(6 a 23 meses y mayores de 65 años) y  en madres vacunadas durante el embarazo 
#(menores de 6 meses)
#===============================================================================


colnames(data)

#===============================================================================
# Selecciono columnas a estudiar
#===============================================================================

vacunacion <- data_principal %>%
  select(EDAD_UC_IRAG, VAC_ANTIGRIPAL, VAC_ANTIGRIPAL_MATERNA, VAC_VSR, CLASIFICACION_MANUAL) 
# Armo un vector para agrupar los grupos de edad de menores de 6 meses

menores_seis_meses <- c("0 a 2 Meses", "3 a 5 Meses")


#Primero aplico a la vacunación materna antigripal

vacunacion_materna <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% menores_seis_meses) %>%
  mutate(Vac_Materna_Antigripal = case_when(VAC_ANTIGRIPAL_MATERNA %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADA",
                                 VAC_ANTIGRIPAL_MATERNA == "MADRE NO VACUNADA" ~ "NO VACUNADA",
                                 TRUE ~ NA_character_)) %>%
  filter(!is.na(Vac_Materna_Antigripal)) %>%
  count(EDAD_UC_IRAG, Vac_Materna_Antigripal) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)*100.1),
         label = paste0(Porcentaje, "%")) %>%
  
  ggplot(aes(x = EDAD_UC_IRAG, y = Porcentaje, fill = Vac_Materna_Antigripal)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5 , fontface = "bold") +
  scale_fill_manual(values = c("NO VACUNADA" = "#BCBEC0","VACUNADA" = "#f7941e"),
                    name = "Vacunacion materna antigripal") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c (0, 101), expand =  c(0 , 0)) +
  labs(title = "Porcentaje de cobertura de vacunación materna antigripal en menores de seis meses",
       x = "Grupo de edad (meses)",
       y = "Porcentaje de pacientes") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 11))

vacunacion_materna


# Ahora aplico a la vacunación materna por VSR

unique(vacunacion$VAC_VSR)
unique(vacunacion$EDAD_UC_IRAG)

vacunacion_materna_vsr <- vacunacion %>%
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
  mutate(Porcentaje = round(n/sum(n)*100.1),
         label = paste0(Porcentaje, "%")) %>%
  
  ggplot(aes(x = EDAD_UC_IRAG, y = Porcentaje, fill = vacuna_materna_sincicial)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5 , fontface = "bold") +
  scale_fill_manual(values = c("NO VACUNADA VSR" = "#BCBEC0","MADRE VACUNADA" = "#009E73"),
                    name = "Vacunacion materna VSR") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c (0, 101), expand =  c(0 , 0)) +
  labs(title = "Porcentaje cobertura de vacunación materna VSR en menores de seis meses",
       x = "Grupo de edad (meses)",
       y = "Porcentaje de pacientes") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 11))

  
  
  vacunacion_materna_vsr
  
  
#=========================================================================== 
# ESTUDIO LOS NIÑOS VACUNADOS DE 6 A 23 MESES Y ADULTOS MAYORES DE 65 años
#===========================================================================

  
unique(data$VAC_ANTIGRIPAL)
unique(data$EDAD_UC_IRAG)

find_na(data$VAC_ANTIGRIPAL)

table(data$VAC_ANTIGRIPAL, useNA = "always")
#=======================================================================
#aplico vector de grupos de edad
#=======================================================================


entre_seis_y_23_meses <- c("6 a 11 Meses", "12 a 23 Meses")

mayores_65_años <- c("65 a 69 Años", "70 a 74 Años", "75 y más Años")

# Se estudia los vacunados por dos grupos de edad

# Primero entre 6 a 23 meses

vacunacion_6_23_meses <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% entre_seis_y_23_meses) %>%
  mutate(vac_antigripal_niños= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                    VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                    TRUE ~ NA_character_)) %>%
  filter(!is.na(vac_antigripal_niños)) %>%
  count(EDAD_UC_IRAG, vac_antigripal_niños) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)*100.1),
         label = paste0(Porcentaje, "%")) %>%
  
  ggplot(aes(x = EDAD_UC_IRAG, y = Porcentaje, fill = vac_antigripal_niños)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5 , fontface = "bold") +
  scale_fill_manual(values = c("NO VACUNADO" = "#BCBEC0","VACUNADO" = "#f7941e"),
                    name = "Vacunacion antigripal") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c (0, 101), expand =  c(0 , 0)) +
  labs(title = "Porcentaje de cobertura de vacunación  antigripal en niños de 6 a 23 meses",
       x = "Grupo de edad (meses)",
       y = "Porcentaje de pacientes") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 11))

vacunacion_6_23_meses
  
# Mayores de 65 años

vacunacion_mayores_65 <- vacunacion %>%
  filter(EDAD_UC_IRAG %in% mayores_65_años) %>%
  filter(VAC_ANTIGRIPAL != "SIN DATO") %>%
  filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiologia") %>%
  mutate(vac_antigripal_adultos= case_when(VAC_ANTIGRIPAL %in% c("CONSTATADA", "REFERIDA") ~ "VACUNADO",
                                   VAC_ANTIGRIPAL == "NO VACUNADO" ~ "NO VACUNADO",
                                   TRUE ~ NA_character_)) %>%
  filter(!is.na(vac_antigripal_adultos)) %>%
  count(EDAD_UC_IRAG, vac_antigripal_adultos) %>%
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)*100.1),
         label = paste0(Porcentaje, "%")) %>%
  
  ggplot(aes(x = EDAD_UC_IRAG, y = Porcentaje, fill = vac_antigripal_adultos)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5 , fontface = "bold") +
  scale_fill_manual(values = c("NO VACUNADO" = "#BCBEC0","VACUNADO" = "#f7941e"),
                    name = "Vacunacion antigripal") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c (0, 101), expand =  c(0 , 0)) +
  labs(title = "Porcentaje de cobertura de vacunación  antigripal en adultos mayores de 65 años",
       x = "Grupo de edad (años)",
       y = "Porcentaje de pacientes") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 11))


vacunacion_mayores_65





  
  
  







