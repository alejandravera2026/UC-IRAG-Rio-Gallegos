#=========================================================================
# Objetivo: 4 Caracterizar las comorbilidades por IRAG e IRAGe grupo etario
#==========================================================================

# Se crea una columna  de comorbilidad

unique (data$PRESENCIA_COMORBILIDADES)

tabla_comorbilidad_grupo <- data %>%
  mutate(COMORBILIDAD = case_when(
    PRESENCIA_COMORBILIDADES == 1  ~ "SI", 
    PRESENCIA_COMORBILIDADES == 2  ~ "NO",
    PRESENCIA_COMORBILIDADES == 9  ~ "SIN DATOS",
    TRUE ~ "SIN DATO"
  ))

# Veo datos faltantes en comorbilidad

sum(is.na(tabla_comorbilidad_grupo$COMORBILIDAD))
  
# Selecciono la columna a estudiar sobre comorbilidad

colnames(tabla_comorbilidad_grupo)

tabla_comorbilidad_grupo <- tabla_comorbilidad_grupo %>%
  select(EDAD_DIAGNOSTICO, COMORBILIDAD)


# Se crea tabla de comorbilidad por grupo de edad establecidos
           
tabla_comorbilidad_grupo_edad <- tabla_comorbilidad_grupo %>%
  filter(COMORBILIDAD %in% c("SI", "NO", "SIN DATOS"))%>%
  mutate(grupo_etario_resumen = case_when(
    EDAD_DIAGNOSTICO >= 0 & EDAD_DIAGNOSTICO < 2 ~ "Menor de 2 años",
    EDAD_DIAGNOSTICO >= 2 & EDAD_DIAGNOSTICO < 15 ~ "2 a 14 años",
    EDAD_DIAGNOSTICO >= 15 & EDAD_DIAGNOSTICO < 65 ~ "15 a 64 años",
    EDAD_DIAGNOSTICO >= 65 ~ "Mayor de 65 años",
    TRUE ~ NA_character_))%>%
  mutate(grupo_etario_resumen = factor(grupo_etario_resumen, levels = c(
    "Menor de 2 años", "2 a 14 años", "15 a 64 años", "Mayor de 65 años"))) 

#Chequear si quedó bien ese grupo etario resumen

table(tabla_comorbilidad_grupo_edad$grupo_etario_resumen, useNA = "always")

# Chequear la comorbilidad cuantos sin datos

table(tabla_comorbilidad_grupo_edad$COMORBILIDAD, useNA = "always")


# Se elabora la tabla de presencia o no de comorbilidad por grupos de edad
# establecidos

tabla_comorbilidad_grupo_edad <- tabla_comorbilidad_grupo_edad %>%
  count(grupo_etario_resumen,COMORBILIDAD) %>%
  group_by(grupo_etario_resumen) %>%
  mutate(Porcentaje = round(n/sum(n)*100.1 ),
         label= paste0(Porcentaje, "%")) %>%

ggplot(aes(x = grupo_etario_resumen, y = Porcentaje, fill = COMORBILIDAD)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4.5 , fontface = "bold") +
  scale_fill_manual(values = c("NO" = "#BCBEC0","SI" = "#FDB913", "SIN DATOS"= "#8B5A2B"),
                    name = "Comorbilidad") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c (0, 101), expand =  c(0 , 0)) +
  labs(title = "Porcentaje de comorbilidades por grupo de edad",
       x = "Grupo de edad",
       y = "Porcentaje de pacientes") +
  theme_minimal() +
  theme(legend.position = "top",
        panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 11))

tabla_comorbilidad_grupo_edad

###################################
##################################

tabla_comorbilidad_gt <- data %>%
  group_by(CLASIFICACION_MANUAL, PRESENCIA_COMORBILIDADES) %>%
  summarise(casos = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = PRESENCIA_COMORBILIDADES,
    values_from = casos,
    values_fill = 0
  ) %>%
  mutate(
    total = `1` + `2` + `9`,
    `1` = round(`1` / total * 100, 1),
    `2` = round(`2` / total * 100, 1),
    `9` = round(`9` / total * 100, 1)
  ) %>%
  select(-total) %>%
  gt() %>%
  cols_label(
    CLASIFICACION_MANUAL = "Clasificación",
    `1` = "Con comorbilidades (%)",
    `2` = "Sin comorbilidades (%)",
    `9` = "Sin datos (%)"
  ) %>%
  cols_align(align = "center") %>%
  tab_header(
    title = "Presencia de comorbilidades",
    subtitle = "Período SE 8 de 2024 a SE 2025"
  )

tabla_comorbilidad_gt
