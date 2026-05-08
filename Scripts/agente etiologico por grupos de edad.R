# Objetivo 10: Describir la distribución del agente etiológico según grupo etario 
# severidad clínica (ingreso a UTI y fallecimiento), a fin de caracterizar el 
# comportamiento epidemiológico de los casos hospitalizados. 

# Selecciono las columnas a estudiar

agente_etiologico_10 <- data %>%
  select(EDAD_UC_IRAG, INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL, CUIDADO_INTENSIVO, FALLECIDO)

# Distribución del agente etiológico según grupo etario

unique(agente_etiologico_10$INFLUENZA_FINAL)
unique(agente_etiologico_10$VSR_FINAL)
unique(agente_etiologico_10$COVID_19_FINAL)

grafico_virus <- agente_etiologico_10 %>%
  filter(!is.na(EDAD_UC_IRAG)) %>%
  pivot_longer(cols = c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
                names_to = "Virus",
                values_to ="Resultado") %>%
  filter(Resultado != "Sin resultado") %>%
  filter(Resultado != "Negativo") %>%
  mutate(Categoria = case_when( Virus == "INFLUENZA_FINAL" & Resultado != "Negativo" ~ Resultado,
                               Virus == "VSR_FINAL" & Resultado == "VSR" ~ "VSR",
                               Virus == "VSR_FINAL" & Resultado == "VSR A" ~ "VSR A",
                               Virus == "VSR_FINAL" & Resultado == "VSR B" ~ "VSR B",
                               Virus == "COVID_19_FINAL" & Resultado == "Positivo" ~ "COVID 19 Positivo",
                               TRUE ~ NA_character_)) %>%
    filter(!is.na(Categoria)) %>%
  #Cuento categoria por grupo de edad
  count(EDAD_UC_IRAG, Categoria) %>%
  # porcentaje del total de determinaciones por grupo de edad
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)* 100,1)) %>%
  ungroup()
  
  ggplot(grafico_virus, aes(x = factor(EDAD_UC_IRAG), y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Porcentaje > 3, paste0(round(Porcentaje, 0), "%"),"")),
            position = position_stack(vjust = 0.5),
            size = 2.5, color = "white") +
  scale_fill_manual(values = c("Influenza A H1N1" = "#e41a1c",
                               "Influenza A H3N2" = "#377eb8",
                               "Influenza A (sin subtipificar)" = "#4daf4a",
                               "Influenza B Victoria" = "#984ea3",
                               "Influenza B (sin linaje)" = "#ff7f00",
                               "Influenza positivo-Sin Tipo" = "#ffff33",
                               "VSR"= "#a65628",
                               "VSR A" = "#f781bf",
                               "VSR B" = "#D32F2F",
                               "COVID 19 Positivo" = "#C2185b"), name = "Agente etiológico") +
  labs(title = "Distribución de los agentes etiológicos por grupos de edad",
       caption = "Fuente SNVS 2.0",
       x= "Grupos de Edad",
       y = "Porcentaje de determinaciones",
       fill= "Agente etiológico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold")) +
  guides(fill = guide_legend(ncol = 1))

#=======================================================================
# Determino los agentes etiológicos que ingresan a cuidados intensivos
#=======================================================================
  
grafico_uti <- agente_etiologico_10 %>%
  filter(!is.na(EDAD_UC_IRAG) ,
                CUIDADO_INTENSIVO == "SI") %>%  
  pivot_longer(cols = c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
               names_to = "Virus",
               values_to ="Resultado") %>%
  filter(Resultado != "Sin resultado") %>%
  mutate(Categoria = case_when(Virus == "INFLUENZA_FINAL" & Resultado == "Negativo" ~ "Influenza negativo",
                               Virus == "INFLUENZA_FINAL" & Resultado != "Negativo" ~ Resultado,
                               Virus == "VSR_FINAL" & Resultado == "Negativo" ~ "VSR negativo",
                               Virus == "VSR_FINAL" & Resultado == "VSR" ~ "VSR",
                               Virus == "VSR_FINAL" & Resultado == "VSR A" ~ "VSR A",
                               Virus == "VSR_FINAL" & Resultado == "VSR B" ~ "VSR B",
                               Virus == "COVID_19_FINAL" & Resultado == "Negativo" ~ "COVID negativo",
                               Virus == "COVID_19_FINAL" & Resultado == "Positivo" ~ "COVID 19 Positivo",
                               TRUE ~ NA_character_)) %>%
   filter(!is.na(Categoria)) %>%
  #Cuento categoria por grupo de edad
  count(EDAD_UC_IRAG, Categoria) %>%
  # porcentaje del total de determinaciones por grupo de edad
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)* 100,1)) %>%
  ungroup()

# Paso a negativos solo los grises
grafico_uti <- grafico_uti%>%
  mutate(Porcentaje_grafico = case_when(Categoria %in% c("Influenza negativo", "VSR negativo",
                                                         "COVID negativo")~ -Porcentaje,
                                        TRUE ~ Porcentaje))
# Ordeno los factores para que los grises queden abajo

orden_categoria<- c("Influenza negativo", "VSR negativo", "COVID negativo", 
                    "Influenza A H1N1", "Influenza B (sin linaje)", "Influenza A H3N2",
                    "Influenza positivo- Sin Tipo", "Influenza A (sin subtipificar", "
                    VSR", "VSR A", "VSR B", "COVID 19 positivo")
grafico_uti$Categoria <- factor(grafico_uti$Categoria, levels = orden_categoria)

ggplot(grafico_uti, aes(x = factor(EDAD_UC_IRAG), y = Porcentaje_grafico, fill = Categoria))+
geom_bar(stat = "identity", position = "stack", width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth =0.8) +
  scale_y_continuous(labels = function(x) paste0(abs(x), "%"),
                     name = "% determinaciones") +
  scale_fill_manual(values = c("Influenza negativo" = "#9e9e9e",
                               "VSR negativo" = "#616161",
                               "COVID negativo" = "#bdbdbd",
                               "Influenza A H1N1" = "#e41a1c",
                               "Influenza A H3N2" = "#377eb8",
                               "Influenza A (sin subtipificar)" = "#4daf4a",
                               "Influenza B Victoria" = "#984ea3",
                               "Influenza B (sin linaje)" = "#ff7f00",
                               "Influenza positivo-Sin Tipo" = "#ffff33",
                               "VSR"= "#a65628",
                               "VSR A" = "#f781bf",
                               "VSR B" = "#D32F2F",
                               "COVID 19 Positivo" = "#C2185b"), name = "Agente etiológico") +
  labs(title = "Distribución de los agentes etiológicos en cuidados intensivos por grupos de edad",
       subtitle = "Período: Desde semana 18/2024 hasta semana 8/2026",
       caption = "Fuente SNVS 2.0",
       x= "Grupos de Edad",
       y = "Porcentaje de determinaciones",
       fill= "Agente etiológico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        legend.position = "right",
        panel.grid.minor = element_blank())

#=============================================================================
 # Determino los fallecidos por agente etiológico y por grupo de edad
#=============================================================================
grafico_fallecido <- agente_etiologico_10 %>%
  filter(!is.na(EDAD_UC_IRAG) ,
         FALLECIDO == "SI") %>%  
  pivot_longer(cols = c(INFLUENZA_FINAL, VSR_FINAL, COVID_19_FINAL),
               names_to = "Virus",
               values_to ="Resultado") %>%
  filter(Resultado != "Sin resultado") %>%
  mutate(Categoria = case_when ( Virus == "INFLUENZA_FINAL" & Resultado == "Negativo" ~ "Influenza negativo",
                                 Virus == "INFLUENZA_FINAL" & Resultado != "Negativo" ~ Resultado,
                                 Virus == "VSR_FINAL" & Resultado == "Negativo" ~ "VSR negativo",
                                 Virus == "VSR_FINAL" & Resultado == "VSR" ~ "VSR",
                                 Virus == "VSR_FINAL" & Resultado == "VSR A" ~ "VSR A",
                                 Virus == "VSR_FINAL" & Resultado == "VSR B" ~ "VSR B",
                                 Virus == "COVID_19_FINAL" & Resultado == "Negativo" ~ "COVID negativo",
                                 Virus == "COVID_19_FINAL" & Resultado == "Positivo" ~ "COVID 19 Positivo",
                                 TRUE ~ NA_character_)) %>%
      filter(!is.na(Categoria)) %>%
  #Cuento categoria por grupo de edad
  count(EDAD_UC_IRAG, Categoria) %>%
  # porcentaje del total de determinaciones por grupo de edad
  group_by(EDAD_UC_IRAG) %>%
  mutate(Porcentaje = round(n/sum(n)* 100,1)) %>%
  ungroup()

ggplot(grafico_fallecido, aes(x = factor(EDAD_UC_IRAG), y = Porcentaje, fill = Categoria)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = ifelse(Porcentaje > 3, paste0(round(Porcentaje, 0), "%"),"")),
            position = position_stack(vjust = 0.5),
            size = 2.5, color = "white") +
  scale_fill_manual(values = c("Influenza negativo" = "#9e9e9e",
                               "VSR negativo" = "#616161",
                               "COVID negativo" = "#bdbdbd",
                               "Influenza A H1N1" = "#e41a1c",
                               "Influenza A H3N2" = "#377eb8",
                               "Influenza A (sin subtipificar)" = "#4daf4a",
                               "Influenza B Victoria" = "#984ea3",
                               "Influenza B (sin linaje)" = "#ff7f00",
                               "Influenza positivo-Sin Tipo" = "#ffff33",
                               "VSR"= "#a65628",
                               "VSR A" = "#f781bf",
                               "VSR B" = "#D32F2F",
                               "COVID 19 Positivo" = "#C2185b"
), name = "Agente etiológico") +
  labs(title = "Distribución de los agentes etiológicos en pacientes fallecidos por grupos de edad",
       subtitle = "Përíodo: Desde semana 18/2024 hasta semana 8/2026",
       caption = "Fuente SNVS 2.0",
       x= "Grupos de Edad",
       y = "Porcentaje de determinaciones",
       fill= "Agente etiológico") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right",
        plot.title = element_text(face = "bold")) +
  guides(fill = guide_legend(ncol = 1))


  