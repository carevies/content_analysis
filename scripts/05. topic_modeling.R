######### Topic modeling

rm(list = ls()) 

library(stm)
library(tidyverse)
library(textstem)

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

#Muestra

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


#Letra ESE
library(stm)
library(tidyverse)

# Suponiendo que letra_ese tiene una columna 'texto' y tus variables de metadata (ajusta si es necesario)
# Si tienes filas vacías o con NA en texto:
letra_ese_clean <- letra_ese %>% filter(!is.na(texto) & texto != "")

# Procesamiento del texto con textProcessor()
procesado <- textProcessor(
  documents = letra_ese_clean$texto,
  metadata = letra_ese_clean,
  language = "spanish"
)

# Preparar documentos para STM
preparado <- prepDocuments(
  documents = procesado$documents,
  vocab = procesado$vocab,
  meta = procesado$meta
)

# Ajustar el modelo STM (puedes quitar 'prevalence' si no quieres covariables)
modelo <- stm(
  documents = preparado$documents,
  vocab = preparado$vocab,
  K = 6,  # Número de temas
  data = preparado$meta,
  max.em.its = 75,
  init.type = "Spectral"
)

# Ver los temas
labelTopics(modelo)

# Gráfica resumen de temas
plot(modelo, type = "summary")
