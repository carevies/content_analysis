######### Topic modeling

rm(list = ls()) 

library(stm)
library(tidyverse)
library(textstem)

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

# Cargar base de datos
muestra <- read.csv("muestra.csv")

# Tomar sólo encabezado (HD) y cuerpo (LP)
muestra <- muestra %>%
  mutate(texto_completo = paste(HD, LP, sep = ". "))

# Procesamiento del texto con textProcessor()
procesado <- textProcessor(
  documents = muestra$texto_completo,
  metadata = muestra,
  language = "spanish"
)

# Preparar documentos para STM
preparado <- prepDocuments(
  documents = procesado$documents,
  vocab = procesado$vocab,
  meta = procesado$meta
)

# Ajustar el modelo STM
modelo <- stm(
  documents = preparado$documents,
  vocab = preparado$vocab,
  K = 6,  # Número de temas
  prevalence = ~ anio + SN,  # Ajusta a tus variables de metadata
  data = preparado$meta,
  max.em.its = 75,
  init.type = "Spectral"
)

# Ver los temas
labelTopics(modelo)

# Gráfica
plot(modelo, type = "summary")