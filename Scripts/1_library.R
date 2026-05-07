# =============================================================================
# SCRIPTS 1 - CONFIGURACIÓN GENERAL
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
# Librerías, identidad visual y recursos gráficos
# =============================================================================

# 1- LIBRERÍAS ------------------------------------------------------------

library(pacman)

pacman::p_load(
  dplyr,        # transformación de datos
  lubridate,    # trabajo con fechas
  dlookr,       # exploración de datos
  readr,        # lectura de archivos
  readxl,       # lectura de archivos. xlsx
  writexl,      # exportar archivos .xlsx
  gt,           # tablas 
  gtable,       # tablas
  tidyverse,    # colección de paquetes tidy
  ggplot2,      # gráficos
  highcharter,  # gráficos interactivos
  tidyr,        # ordenamiento de datos 
  stringr,      # trabajo con texto
  janitor,      # limpieza de nombres / tablas
  patchwork,    # combinar gráficos 
  png,
  grid
)


# 2- IDENTIDAD VISUAL INSTITUCIONAL  --------------------------------------

# Colores institucionales principales Gobierno de Santa Cruz 

 azul <- "#252C61" 
 celeste <- "#7EC8E6"
 amarillo <- "#FDB913"
 gris <- "#BCBEC0"

# Colores extendidos para uso epidemiológico controlado
 
 verde_epi <- "#009E73"
 rojo_epi <- "#C62828"
 naranja_epi <- "#E69f00"
 
#  Paletas de uso específico
 
 colores_irag <- c(
   "IRAG"= azul,
   "IRAGE"= celeste
   )
 
 colores_severidad <- c(
   "No ingreso a UTI" = gris,
   "Ingreso a UTI" = amarillo,
   "Fallecido" = rojo_epi
   )
 
 colores_comorbilidad <- c(
   "Sin comorbilidad" = "gris", 
   "Con comorbilidad" = "amarillo"
   )
   
 colores_vacunacion <- c(
   "No Vacunado" = "gris",
   "Influenza" = "#f7941e",
   "VSR" = "verde_epi"
 )
 
 colores_virus <- c(
   "SARS-CoV-2" = rojo_epi,
   "Influenza" = naranja_epi, 
   "VSR" = verde_epi
 )
 
 
# 3- LOGOS INSTITUCIONALES  -----------------------------------------------

# a- Crear una carpeta que se llame "Logos" 
# b- Guardar los logos que estan en el drive compartido 
# c- Ejecutar las siguientes líneas de código.  

 logo_gobierno <- "Logos/Escudo.png"

 logo_hospital <- "Logos/hospital.png" 
 

 
 
 
 
 
 