
# Filtro para que no realice el análisis casos estudiados

unique(data$CLASIFICACION_MANUAL)
data1 <- data %>% filter(CLASIFICACION_MANUAL !="Caso invalidado por epidemiología" &
                           ANIO_MIN_INTERNACION == 2025)
#Frecuencia de datos-2025
# Frecuencia absoluta

Casos_IRAG_2025 <- sum(data1$CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)",
                  ra.rm = TRUE)

Casos_IRAGE_2025 <- sum(data1$CLASIFICACION_MANUAL=="IRAG extendida", ra.rm = TRUE)

casos_totales_2025 <- sum(Casos_IRAG_2025 + Casos_IRAGE_2025)

# Frecuencia relativa

proporcion_IRAG_2025 <-round((Casos_IRAG_2025/casos_totales_2025)*100,1)
proporcion_IRAGE_2025 <-round((Casos_IRAGE_2025/casos_totales_2025)*100,1)
 #Año 2024
# Se filran los casos invalidados

data2 <- data %>% filter(CLASIFICACION_MANUAL != "Caso invalidado por epiemdiología" &
                           ANIO_MIN_INTERNACION == 2024)
# Se suman los casos IRAG e IRAG extendida y se realiza la proporción

Casos_IRAG_2024 <- sum(data2$CLASIFICACION_MANUAL =="Infección respiratoria aguda grave (IRAG)",
                       ra.rm =TRUE)
Casos_IRAGE_2024 <- sum(data2$CLASIFICACION_MANUAL == "IRAG extendida", ra.rm = TRUE)

Casos_totales_2024 <- sum (Casos_IRAG_2024 + Casos_IRAGE_2024)

proporcion_IRAG_2024 <- round((Casos_IRAG_2024/Casos_totales_2024)*100.1)
proporcion_IRAGE_2024 <- round((Casos_IRAGE_2024/Casos_totales_2024)*100.1)

#2026
#Filtrar casos invalidados

data3 <- data %>% filter(CLASIFICACION_MANUAL != "Caso invalidado por epidemiología" & 
                           ANIO_MIN_INTERNACION == 2026)
# Se suman los casos irag e irag extendida y se realiza la proporción

Casos_IRAG_2026 <- sum (data3$CLASIFICACION_MANUAL== "Infección respiratoria aguda grave (IRAG)",
                        ra.rm= TRUE)
Casos_IRAE_2026 <- sum(data3$CLASIFICACION_MANUAL=="IRAG extendida", ra.rm = TRUE)
Casos_Totales_2026 <- sum(Casos_IRAE_2026 + Casos_IRAG_2026)

proporcion_IRAG_2026 <- round((Casos_IRAG_2026/Casos_Totales_2026)*100.1)
proporcion_IRAGE_2026 <- round((Casos_IRAE_2026/Casos_Totales_2026)*100.1)





