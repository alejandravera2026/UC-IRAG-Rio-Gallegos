# =============================================================================
# SCRIPTS 1 - CONFIGURACIÓN GENERAL
# UNIDAD CENTINELA DE INFECCIONES RESPIRATORIAS AGUDAS 
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
  tidyverse,    # colección de paquetes tidye
  ggplot2,      # gráficos
  highcharter,  # gráficos interactivos
  tidyr,        # ordenamiento de datos 
  stringr,      # trabajo con texto
  janitor,      # limpieza de nombres / tablas
  patchwork,    # combinar gráficos 
  png,          # exportación de imágenes .png   
  grid          # organización y diseño gráfico 
)


