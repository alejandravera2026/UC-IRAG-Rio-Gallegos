# ===========================================================================
# SCRIPTS 5 - PROCESAMIENTO BASE AGRUPADA 
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
# ===========================================================================


# 1- ELIMINO PRIMERA FILA DE LA BASE  -------------------------------------

agrupada <-agrupada [-1,]


# 2- SELECCIONO COLUMNAS A CONVERTIR A NUMERIC  ---------------------------

columnas_numeric <- agrupada %>%
  
  select("ANIO", "SEMANA", 
         "0 a 2 m":"Sin especificar") %>%
  names()


# 3- TRANSFORMO COLUMNAS A FORMATO NUMÉRICO -------------------------------

agrupada <- agrupada %>% 
  
  mutate(across(all_of(columnas_numeric),
                ~ as.numeric(.x)
  )
  )

# 4- SELECCIONO COLUMNAS DE INTERÉS ---------------------------------------

columnas_eliminar <- c("PROVINCIA","FECHAREGISTROENCABEZADO",
                       "ORIGEN","USUARIOREGISTROENCABEZADO")


# 5- SELECCIONO EVENTOS DE INTERÉS ----------------------------------------

unique(agrupada$NOMBREEVENTOAGRP)

agrupada <- agrupada %>% 
  
  mutate(NOMBREEVENTOAGRP = str_replace_all
         (NOMBREEVENTOAGRP,
           "Casos de IRAG EXTENDIDA entre los ingresados a UCI",
           "Casos de IRAG extendida entre los ingresados a UCI")
  )

evento_agrupado <- c("Pacientes internados por todas las causas",
                     "Casos de IRAG entre los internados",
                     "Casos de IRAG extendida entre los internados",
                     "Defunciones por IRAG","Defunciones por IRAG extendida",
                     "Defunciones totales",
                     "Pacientes ingresados a UCI",
                     "Casos de IRAG entre los ingresados a UCI",
                     "Casos de IRAG extendida entre los ingresados a UCI")



# 6- APLICO LOS FILTROS INDICADOS -----------------------------------------

agrupada <- agrupada %>% 
  
  select(-any_of(columnas_eliminar)
  ) %>%
  
  filter(NOMBREEVENTOAGRP %in% evento_agrupado)


# 7- PIVOTEO DE DATOS A FORMATO LARGO (LONGER) ----------------------------
#para agrupar por SE

agrupada <- agrupada %>% 
  
  pivot_longer(cols = "0 a 2 m":ncol(agrupada),
               names_to = "GRUPO_ETARIO",
               values_to = "CASOS") 


# 8- CONVIERTO CASOS A NUMERIC PARA SUMAR  --------------------------------

agrupada <- agrupada %>%
  
  mutate(CASOS = as.numeric(CASOS)
  )

# 9- B. AGRUPADA A PARTIR DE SE 04 AÑO 2026 -------------------------------
# Selecciono columnas de interés 

agrupada_2026 <- agrupada_2026 %>% 
  select(ANIO,SEMANA,NOMBRE_EVENTO_AGRP,GRUPO,CANTIDAD)


# 10- APLICO FILTROS (HASTA SE 8 DEL 2026) --------------------------------

agrupada_2026 <- agrupada_2026 %>%
  
  filter( # Desde el inicio del periodo de análisis
    (ANIO > ANIO_MINIMO | 
       (ANIO == ANIO_MINIMO & SEMANA >= SEMANA_MINIMA)
    ) &
      
      # Hasta el final del periodo de análisis
      (ANIO < ANIO_MAXIMO | 
         (ANIO == ANIO_MAXIMO & SEMANA <= SEMANA_MAXIMA))
  )

# 11- FILTRO EVENTOS DE INTERÉS -------------------------------------------

agrupada_2026 <- agrupada_2026 %>% 
  
  filter(NOMBRE_EVENTO_AGRP %in% evento_agrupado)

# Filtro grupo de edad

agrupada_2026 <- agrupada_2026 %>% 
  
  filter(GRUPO != "Todos los rangos")

# Unifico categorías de grupo de edad

agrupada_2026 <- agrupada_2026 %>%
  
  mutate(GRUPO = str_replace_all(GRUPO, 
                                 ">= a 75", ">= a 75 años"),
         GRUPO = str_replace_all(GRUPO, 
                                 "Edad sin esp.", "Sin especificar")
  )

# 12- RENOMBRO COLUMNAS  --------------------------------------------------
# para que coincidan con el drive agrupado y asi unir BD agrupadas

agrupada_2026 <- agrupada_2026 %>% 
  rename ("CASOS" = "CANTIDAD",
          "GRUPO_ETARIO" = "GRUPO",
          "NOMBREEVENTOAGRP" = "NOMBRE_EVENTO_AGRP")


# uno bases de datos: drive agrupado y exportación SNVS

agrupada <- agrupada %>% 
  rbind(agrupada_2026)


# 13- AGRUPO TABLA POR SE, EVENTO Y AÑO -----------------------------------

tabla_resumen <- agrupada %>% 
  
  group_by(ANIO,SEMANA,NOMBREEVENTOAGRP) %>%
  summarise(CASOS = sum(CASOS,na.rm =T)) %>%
  ungroup()

# Creo SEPI (une año con SE) para etiquetar ejes
tabla_resumen <- tabla_resumen %>% 
  
  mutate(SEPI = paste(ANIO,"-",SEMANA)
  )

# 14- PASO BASE A FORMATO ANCHO (WIDER) -----------------------------------
# para calcular proporciones

tabla_resumen <- tabla_resumen %>% 
  pivot_wider(names_from = NOMBREEVENTOAGRP,
              values_from = CASOS)


# 15- CALCULO PROPROCIONES SOBRE BASE AGRUPADA ----------------------------
# Creo variable fallecidos por IRAG e IRAGe 

tabla_resumen <- tabla_resumen %>% 
  
  mutate(FALLECIDOS_IRAG = (`Defunciones por IRAG` + 
                              `Defunciones por IRAG extendida`),
         UCI_IRAG_IRAGE = (`Casos de IRAG entre los ingresados a UCI`+
                             `Casos de IRAG extendida entre los ingresados a UCI`)
  ) 

# 16- PROPORCIÓN IRAG E IRAGe / INGRESOS TOTALES  -------------------------

colnames(tabla_resumen)

tabla_resumen <- tabla_resumen %>% 
  mutate(PROPORCION_IRAG = round(
    (`Casos de IRAG entre los internados`/
       `Pacientes internados por todas las causas`)*100,1),
    
    PROPORCION_IRAGE = round(
      (`Casos de IRAG extendida entre los internados`/
         `Pacientes internados por todas las causas`)*100,1),
    
    PROPORCION_INTERNADOS_OTRAS_CAUSAS = (100-(PROPORCION_IRAG + 
                                                 PROPORCION_IRAGE)
    ),
    PROPORCION_FALLECIDOS_IRAG = round(
      (`Defunciones por IRAG`/
         `Defunciones totales`)*100,1),
    PROPORCION_FALLECIDOS_IRAGE = round(
      (`Defunciones por IRAG extendida`/
           `Defunciones totales`)*100,1),
    PROPORCION_FALLECIDOS_OTRAS_CAUSAS = (100 - (PROPORCION_FALLECIDOS_IRAG + 
                                                   PROPORCION_FALLECIDOS_IRAGE)),
    IRAG_UCI = round(
      (`Casos de IRAG entre los ingresados a UCI`/
         `Pacientes ingresados a UCI`)*100,1),
    IRAGE_UCI = round(
      (`Casos de IRAG extendida entre los ingresados a UCI`/
         `Pacientes ingresados a UCI`)*100,1),
     PROPORCION_UCI_OTRAS_CAUSAS = (100-(IRAG_UCI + IRAGE_UCI)
    )
  )

# 13- PARA ELIMINAR DE LOS GRÁFICOS LAS SE CON ERRORES DE CARGA  ----------

tabla_resumen <- tabla_resumen %>% 
  
  filter(PROPORCION_UCI_OTRAS_CAUSAS >= 0)


