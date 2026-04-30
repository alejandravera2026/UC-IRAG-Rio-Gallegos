# Importación de datos actualizados en el drive

data <- read.csv2("Data/UC_IRAG_EST20892.csv",sep = ";", encoding = "latin1")

agrupada <- read_xlsx("Data/UC_IRAG_AGRUPADA.xlsx")


agrupada_2026  <- read.csv("Data/AG_UC_IRAG_[EST20892]_2026_04_16.csv",sep = ";", encoding = "latin1")


# ===== PARÁMETROS TEMPORALES PARA EL ANÁLISIS ======

#Defino parámetros temporales de análisis (establecidos en el plan)

ANIO_MINIMO <- 2024

SEMANA_MINIMA <- 18

ANIO_MAXIMO <- 2026

SEMANA_MAXIMA <- 8
