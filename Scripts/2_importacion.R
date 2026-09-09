#=============================================================================== 
# SCRPT 2 - IMPORTACIÓN DE DATOS
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
# Lectura de bases, control inicial y parámetros temporales
# ==============================================================================

# 1- IMPORTACIÓN DE ARCHIVOS ----------------------------------------------

data <- read.csv2("Data/UC_IRAG_EST20892.csv",
                  sep = ";",
                  encoding = "latin1")
data1 <- read.csv2("Data/UC_IRAG_SANTA_CRUZ.csv",
                   sep = ";",
                   encoding = "latin1")

agrupada <- read_xlsx("Data/UC_IRAG_AGRUPADA.xlsx")

agrupada_2026  <- read.csv("Data/AG_UC_IRAG_[EST20892]_2026_04_16.csv",
                           sep = ";", 
                           encoding = "latin1")


# 2- CONTROL INICIAL DE OBJETOS -------------------------------------------

names(data)
names(agrupada)
names(agrupada_2026)


# 3- NORMALIZACIÓN DE VARIABLES -------------------------------------------

data <- data %>% 
  mutate(
    ANIO_MIN_INTERNACION=
      as.numeric(ANIO_MIN_INTERNACION),
    SEPI_MIN_INTERNACION = 
      as.numeric(SEPI_MIN_INTERNACION)
  )


data1 <- data1 %>% 
  mutate(
    ANIO_MIN_INTERNACION=
      as.numeric(ANIO_MIN_INTERNACION),
    SEPI_MIN_INTERNACION = 
      as.numeric(SEPI_MIN_INTERNACION)
  )



