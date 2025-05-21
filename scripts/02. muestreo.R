######### Muestreo

rm(list = ls()) 

if (!require("dplyr")) install.packages("dplyr")
if (!require("readr")) install.packages("readr")
library(dplyr)
library(readr)

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

medios <- read_csv("medios.csv")

# Crear columna 'anio' desde la columna DT
medios <- medios %>% mutate(anio = format(DT, "%Y"))

# Definir proporción (~10%)
prop <- 0.10

# Tomar muestra estratificada: ~10% por año, al menos 1 por año
set.seed(123)  # Para que sea reproducible
muestra <- medios %>%
  group_by(anio) %>%
  sample_n(size = max(1, round(n() * prop)), replace = FALSE) %>%
  ungroup()

# Guardar la muestra como CSV
write_csv(muestra, "muestra.csv")
