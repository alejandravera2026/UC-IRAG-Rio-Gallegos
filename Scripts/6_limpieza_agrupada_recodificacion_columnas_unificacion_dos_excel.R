#Procesamiento base agrupada

#Elimino primera fila de la base 

agrupada <-agrupada [-1,]

str(agrupada)

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

unique(agrupada$NOMBREEVENTOAGRP)

agrupada <- agrupada %>% 
  mutate(NOMBREEVENTOAGRP = str_replace_all(NOMBREEVENTOAGRP,
                                            
                                            "Casos de IRAG EXTENDIDA entre los ingresados a UCI",
                                            
                                            "Casos de IRAG extendida entre los ingresados a UCI"))

evento_agrupado <- c("Pacientes internados por todas las causas","Casos de IRAG entre los internados",
                     "Casos de IRAG extendida entre los internados","Defunciones por IRAG","Defunciones por IRAG extendida",
                     "Defunciones totales","Pacientes ingresados a UCI","Casos de IRAG entre los ingresados a UCI","Casos de IRAG extendida entre los ingresados a UCI")



#Aplico los filtros indicados

agrupada <- agrupada %>% select(-any_of(columnas_eliminar)) %>%
  filter(NOMBREEVENTOAGRP %in% evento_agrupado)

#Pivoteo de datos a formato largo (longer) para agrupar por SE

agrupada <- agrupada %>% pivot_longer(cols = "0 a 2 m":ncol(agrupada),
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


# Filtro eventos de interés

agrupada_2026 <- agrupada_2026 %>% filter(NOMBRE_EVENTO_AGRP %in% evento_agrupado)

#Filtro grupo de edad

agrupada_2026 <- agrupada_2026 %>% filter(GRUPO != "Todos los rangos")

#Unifico categorías de grupo de edad

agrupada_2026 <- agrupada_2026 %>%
  mutate(GRUPO = str_replace_all(GRUPO, ">= a 75", ">= a 75 años"),
         GRUPO = str_replace_all(GRUPO, "Edad sin esp.", "Sin especificar"))

#Renombro columnas para que coincidan con el drive agrupado y poder unir bases de datos agrupadas

agrupada_2026 <- agrupada_2026 %>% rename ("CASOS" = "CANTIDAD",
                                           "GRUPO_ETARIO" = "GRUPO",
                                           "NOMBREEVENTOAGRP" = "NOMBRE_EVENTO_AGRP")


#Uno bases de datos: drive agrupado y exportación SNVS

agrupada <- agrupada %>% rbind(agrupada_2026)