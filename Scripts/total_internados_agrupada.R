# Total de casos internados

colnames (agrupada)

unique(agrupada$NOMBREEVENTOAGRP)

evento_internados_totales <- agrupada %>%
  filter(NOMBREEVENTOAGRP == "Pacientes internados por todas las causas")

tabla_internados_totales <- evento_internados_totales %>%
  group_by(SEMANA,ANIO)%>%
  summarise(Total = sum(Total, na.rm = TRUE), .groups = "drop")

tabla_internados_totales$SEMANA <-as.integer(tabla_internados_totales$SEMANA)
tabla_internados_totales$Total <-as.integer(tabla_internados_totales$Total)

tabla_internados_totales <- tabla_internados_totales %>% 
  complete(ANIO, SEMANA = 1:53, fill = list(n=0))%>%
  mutate(SE= str_pad(SEMANA, width = 2, side = "left", pad = "0"))



Totales_2024 <- tabla_internados_totales %>% filter(ANIO== 2024)
Totales_2025 <- tabla_internados_totales %>% filter(ANIO == 2025)
Totales_2026 <- tabla_internados_totales %>% filter(ANIO == 2026)
