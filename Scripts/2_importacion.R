#=============================================================================== 
# SCRPT 2 - IMPORTACIÓN DE DATOS
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS
# Lectura de bases, control inicial y parámetros temporales
# ==============================================================================


# 1- IMPORTACIÓN DE ARCHIVOS ----------------------------------------------

data <- read.csv2("Data/UC_IRAG_EST20892.csv",
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

# Verificación inicial de estructura

str(data$ANIO_MIN_INTERNACION)
str(data$SEPI_MIN_INTERNACION)


# 4- PARÁMETROS TEMPORALES PARA EL ANÁLISIS -------------------------------

ANIO_MINIMO <- 2024
SEMANA_MINIMA <- 18

ANIO_MAXIMO <- 2026
SEMANA_MAXIMA <- 8


# 5- VERIFICACIÓN DE ARCHIVOS CARGADOS ------------------------------------

ls()
