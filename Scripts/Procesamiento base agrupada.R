#Procesamiento base agrupada

#Elimino primera fila de la base 

agrupada <-agrupada [-1,]

str(data_agrupada)

# Selecciono columnas a converir a numeric

columnas_numeric <- agrupada %>%
  select("ANIO", "SEMANA", "0 a 2 m":"Sin especificar") %>%
  names()

#Transformo columnas a formato numérico
agrupada <- agrupada %>% 
  mutate(across(all_of(columnas_numeric), ~ as.numeric(.x)))


#Selecciono de columnas de interés para el análisis de la base agrupada

columnas_eliminar <- c("PROVINCIA","FECHAREGISTROENCABEZADO","ORIGEN","USUARIOREGISTROENCABEZADO")

#Selecciono eventos de interés para el análisis de la base agrupada

evento_agrupado <- c("Pacientes internados por todas las causas","Casos de IRAG entre los internados",
                     "Casos de IRAG extendida entre los internados","Defunciones por IRAG","Defunciones por IRAG extendida",
                     "Defunciones totales")

#Aplico los filtros indicados

agrupada <- agrupada %>% select(-any_of(columnas_eliminar)) %>%
  filter(NOMBREEVENTOAGRP %in% evento_agrupado)

#Pivoteo de datos a formato largo (longer) para agrupar por SE

agrupada <- agrupada %>% pivot_longer(cols = "0 a 2 m":ncol(data_agrupada),
                                      names_to = "GRUPO_ETARIO",
                                      values_to = "CASOS") 

#Convierto CASOS a numeric para poder sumarlos
agrupada <- agrupada %>% mutate(CASOS = as.numeric(CASOS))

#================================================
# ⚠️ BASE CARGA AGRUPADA A PARTIR DE SE 04 AÑO 2026
#===============================================

#Selecciono columnas de interés para el análisis

agrupada_2026 <- agrupada_2026 %>% select(ANIO,SEMANA,NOMBRE_EVENTO_AGRP,GRUPO,CANTIDAD)

# Aplico filtros respetando periodo de análisis (hasta semana 08 del 2026)
agrupada_2026 <- agrupada_2026 %>%
  filter(
    
    #Desde el inicio del periodo de análisis
    (ANIO > ANIO_MINIMO | 
       (ANIO == ANIO_MINIMO & SEMANA >= SEMANA_MINIMA)) &
      
      # Hasta el final del periodo de análisis
      (ANIO < ANIO_MAXIMO | 
         (ANIO == ANIO_MAXIMO & SEMANA <= SEMANA_MAXIMA))
  )


#Renombro columnas para que coincidan con el drive agrupado y poder unir bases de datos agrupadas

agrupada_2026 <- agrupada_2026 %>% rename ("CASOS" = "CANTIDAD",
                                           "GRUPO_ETARIO" = "GRUPO",
                                           "NOMBREEVENTOAGRP" = "NOMBRE_EVENTO_AGRP")


#Uno bases de datos: drive agrupado y exportación SNVS

agrupada <- agrupada %>% rbind(data_agrupada_2026)


#===========================================
#Agrupo tabla por semana, evento y año
#============================================

tabla_resumen <- agrupada %>% group_by(ANIO,SEMANA,NOMBREEVENTOAGRP) %>%
  summarise(CASOS = sum(CASOS,na.rm =T)) %>%
  ungroup()


#Creo SEPI (une año con SE) para etiquetar ejes
tabla_resumen <- tabla_resumen %>% mutate(SEPI = paste(ANIO,"-",SEMANA))

#===============================================================
# Paso base a formato ancho (wider) para calcular proporciones
#===============================================================

tabla_resumen <- tabla_resumen %>% pivot_wider(names_from = NOMBREEVENTOAGRP,
                                               values_from = CASOS)

#===============================================================
# ⚠️ Calculo porporciones sobre la base agrupada
#===============================================================

# Creo variable fallecidos por IRAG e IRAGe 

tabla_resumen <- tabla_resumen %>% mutate(FALLECIDOS_IRAG = (`Defunciones por IRAG` + `Defunciones por IRAG extendida`))

# Proporcion IRAG e IRAGe sobre ingresos totales

colnames(tabla_resumen)

tabla_resumen <- tabla_resumen %>% mutate(PROPORCION_IRAG = round(
  (`Casos de IRAG entre los internados`/`Pacientes internados por todas las causas`)*100,1),
  PROPORCION_IRAGE = round(
    (`Casos de IRAG extendida entre los internados`/`Pacientes internados por todas las causas`)*100,1),
  PROPORCION_INTERNADOS_OTRAS_CAUSAS = (100-(PROPORCION_IRAG + PROPORCION_IRAGE)),
  PROPORCION_FALLECIDOS = round(
    (FALLECIDOS_IRAG/`Defunciones totales`)*100,1))


#================================================================================
# 📊 Grafico interactivo proporcion de casos de IRAG e IRAGe entre los internados   
#================================================================================

curva_internaciones_irag <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_plotOptions(column = list(stacking = "percent",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = tabla_resumen$SEPI, #categorías en eje X
    title = list(text = "Año - Semana")) %>%  #título del eje X) 
  hc_yAxis(
    title = list(text = "Porcentaje de ingresos"),
    labels = list(format = "{value}%"),
    max = 100) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = tabla_resumen$PROPORCION_INTERNADOS_OTRAS_CAUSAS,
    name = "Otras causas",
    color = "lightgrey") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_IRAG,
    name = "IRAG",
    color = "#7fc97f") %>%
  hc_add_series(
    data = tabla_resumen$PROPORCION_IRAGE,
    name = "IRAGe",
    color = "#beaed4")


curva_internaciones_irag 
