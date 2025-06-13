######### Topic modeling

rm(list = ls()) 

library(stm)
library(tidyverse)
library(textstem)
library(readr)
library(dplyr)
library(ggplot2)

setwd("/Users/carlosvillalobos/Library/CloudStorage/OneDrive-UniversitatdeBarcelona/Documents/Analisis de contenido/content_analysis/data")
smos <- read_csv("smos.csv")
muestra <- read_csv("muestra.csv")

### Topic Modeling con Prensa

# Cargar base de datos
muestra <- read.csv("muestra.csv")

# Tomar sólo encabezado (HD) y cuerpo (LP)
muestra <- muestra %>%
  mutate(texto_completo = paste(HD, LP, sep = ". "))

# Procesamiento del texto con textProcessor()
procesado_muestra <- textProcessor(
  documents = muestra$texto_completo,
  metadata = muestra,
  language = "spanish"
)

# Preparar documentos para STM
preparado_muestra <- prepDocuments(
  documents = procesado_muestra$documents,
  vocab = procesado_muestra$vocab,
  meta = procesado_muestra$meta
)

# Ajustar el modelo STM
modelo_muestra <- stm(
  documents = procesado_muestra$documents,
  vocab = procesado_muestra$vocab,
  K = 8,  # Número de temas
  prevalence = ~ anio + SN,  # Variables de metadata
  data = procesado_muestra$meta,
  max.em.its = 75,
  init.type = "Spectral"
)

# Ver los temas
labelTopics(modelo_muestra)

# Gráfica
plot(modelo_muestra, type = "summary")

#Explorar términos de cada topico
labelTopics(modelo_muestra, n = 50)

#EVOLUCION POR AÑO
# Extraer la matriz de probabilidades de tópicos para cada documento
theta_muestra <- as.data.frame(modelo_muestra$theta)

# Añadir columna de año (anio) que viene de los metadatos de los documentos
theta_muestra$anio <- preparado_muestra$meta$anio

# Convertir a formato largo para tidyverse
theta_long_muestra <- theta_muestra %>%
  mutate(doc_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "topic",
    values_to = "probabilidad"
  )

# Calcular la prevalencia promedio de cada topic por año
prevalencia_muestra<- theta_long_muestra %>%
  group_by(anio, topic) %>%
  summarise(media = mean(probabilidad), .groups = "drop")

# Graficar la evolución de la prevalencia de cada topic por año
ggplot(prevalencia_muestra, aes(x = anio, y = media, color = topic, group = topic, shape = topic)) +
  geom_line(size = .25) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7)) + # 8 shapes bien visibles
  labs(
    title = "Evolución topicos en medios",
    x = "Año",
    y = "Prevalencia promedio",
    color = "Topic",
    shape = "Topic"
  ) +
  theme_minimal()

### Topic Modeling con SMOS

# Cargar base de datos
smos <- read.csv("smos.csv")

smos <- smos %>%
  mutate(
    DT = as.Date(DT),
    anio = year(DT))

# Tomar sólo encabezado (HD) y cuerpo (LP)
smos <- smos %>%
  mutate(texto_completo = paste(HD, LP, sep = ". "))

# Procesamiento del texto con textProcessor()
procesado_smos <- textProcessor(
  documents = smos$texto_completo,
  metadata = smos,
  language = "spanish"
)

# Preparar documentos para STM
preparado_smos <- prepDocuments(
  documents = procesado_smos$documents,
  vocab = procesado_smos$vocab,
  meta = procesado_smos$meta
)

# Ajustar el modelo STM
modelo_smos <- stm(
  documents = procesado_smos$documents,
  vocab = procesado_smos$vocab,
  K = 8,  # Número de temas
  prevalence = ~ anio + SN,  # Variables de metadata
  data = procesado_smos$meta,
  max.em.its = 75,
  init.type = "Spectral"
)

# Ver los temas
labelTopics(modelo_smos)

# Gráfica
plot(modelo_smos, type = "summary")

#Explorar términos de cada topico
labelTopics(modelo_smos, n = 50)

#EVOLUCION POR ANIO
theta_smos <- as.data.frame(modelo_smos$theta)

# Añadir columna de año (anio) que viene de los metadatos de los documentos
theta_smos$anio <- preparado_smos$meta$anio

# Convertir a formato largo para tidyverse
theta_long_smos <- theta_smos %>%
  mutate(doc_id = row_number()) %>%
  pivot_longer(
    cols = starts_with("V"),
    names_to = "topic",
    values_to = "probabilidad"
  )

# Calcular la prevalencia promedio de cada topic por año
prevalencia_smos <- theta_long_smos %>%
  group_by(anio, topic) %>%
  summarise(media = mean(probabilidad), .groups = "drop")

# Graficar la evolución de la prevalencia de cada topic por año
ggplot(prevalencia_smos, aes(x = anio, y = media, color = topic, group = topic, shape = topic)) +
  geom_line(size = .25) +
  geom_point(size = 2) +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7)) + # 8 shapes bien visibles
  labs(
    title = "Evolución topicos en smos",
    x = "Año",
    y = "Prevalencia promedio (theta)",
    color = "Topic",
    shape = "Topic"
  ) +
  theme_minimal()

#### COMPARAR TOPICOS COMUNES:

#Preparar dataframes
theta_muestra <- as.data.frame(modelo_muestra$theta)
theta_muestra$anio <- preparado_muestra$meta$anio

theta_smos <- as.data.frame(modelo_smos$theta)
theta_smos$anio <- preparado_smos$meta$anio

#Comparación "Documentación": 2 Muestra y 8 Smos
topic2_muestra <- theta_muestra %>%
  select(V2, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V2, na.rm = TRUE)) %>%
  mutate(topic = "Medios")

topic8_smos <- theta_smos %>%
  select(V8, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V8, na.rm = TRUE)) %>%
  mutate(topic = "SMOS")

#Comparación y Gráfica
comparacion_topics <- bind_rows(topic2_muestra, topic8_smos)

ggplot(comparacion_topics, aes(x = anio, y = prevalencia, color = topic, shape = topic, group = topic)) +
  geom_line(size = .5) +
  geom_point(size = 1) +
  labs(
    title = "Comparación de evolución Topic Documentos",
    x = "Año",
    y = "Prevalencia promedio (theta)",
    color = "Grupo",
    shape = "Grupo"
  ) +
  theme_minimal()

#Comparación "Violencia": 1 Muestra y 6 Smos
topic1_muestra <- theta_muestra %>%
  select(V1, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V1, na.rm = TRUE)) %>%
  mutate(topic = "Medios")

topic6_smos <- theta_smos %>%
  select(V6, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V6, na.rm = TRUE)) %>%
  mutate(topic = "SMOS")

#Comparación y Gráfica
comparacion_topics <- bind_rows(topic1_muestra, topic6_smos)

ggplot(comparacion_topics, aes(x = anio, y = prevalencia, color = topic, shape = topic, group = topic)) +
  geom_line(size = .5) +
  geom_point(size = 1) +
  labs(
    title = "Comparación de evolución Topic Violencia",
    x = "Año",
    y = "Prevalencia promedio (theta)",
    color = "Grupo",
    shape = "Grupo"
  ) +
  theme_minimal()

#Comparación "Salud": 5 Muestra y 1 Smos
topic5_muestra <- theta_muestra %>%
  select(V5, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V5, na.rm = TRUE)) %>%
  mutate(topic = "Medios")

topic1_smos <- theta_smos %>%
  select(V1, anio) %>%
  group_by(anio) %>%
  summarise(prevalencia = mean(V1, na.rm = TRUE)) %>%
  mutate(topic = "SMOS")

#Comparación y Gráfica
comparacion_topics <- bind_rows(topic5_muestra, topic1_smos)

ggplot(comparacion_topics, aes(x = anio, y = prevalencia, color = topic, shape = topic, group = topic)) +
  geom_line(size = .5) +
  geom_point(size = 1) +
  labs(
    title = "Comparación de evolución Topic Violencia",
    x = "Año",
    y = "Prevalencia promedio (theta)",
    color = "Grupo",
    shape = "Grupo"
  ) +
  theme_minimal()
