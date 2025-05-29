######### Análisis descriptivo

install.packages(c("dplyr", "tidytext", "stringr", "text2vec", "tibble", "tokenizers", "purrr"))
library(dplyr)
library(tidytext)
library(stringr)
library(text2vec)
library(tibble)
library(tokenizers)
library(purrr)
library(readr)
library(Matrix)


# Tus listas combinadas
terminos <- c(
  # Lista de conceptos
  "personas trans", "población trans", "transgénero", "transexual", "transexuales", "travesti",
  "travestis", "transvesti", "transvestis", "cambio de sexo", "reasignación de sexo", "sexo asignado",
  "reasignación de género", "género autopercibido", "cirugía de cambio de sexo", "disforia de género",
  "identidad trans", "identidad de género", "derechos trans", "derechos de los trans", "transfobia",
  "discriminación trans", "odio trans", "violencia trans", "feminicidio trans", "personas no binarias",
  "no binario", "no binaria", "no binarie", "género no binario", "género fluido", "genderqueer", "queer",
  "tercer género", "magistrade", "pronombres no binarios", "representación trans", "visibilidad trans",
  "marchas trans", "orgullo trans", "movimiento trans", "activismo trans", "colectivos trans", "ONG trans",
  "Pride", "Marcha del Orgullo", "Orgullo Gay", "expresión de género", "reconocimiento legal trans",
  "cambio de identidad de género", "ley de identidad de género", "mujeres trans", "hombres trans",
  "infancias trans", "salud trans", "hormonización trans", "terapia de reemplazo hormonal",
  "Clínica Condesa", "Grupo Eon", "Inteligencia Transgenérica", "Frente Pro Derechos Transgénero y Transexuales",
  "Red de Trabajo Trans", "Coalisión T47", "Almas Cautivas", "Impulso Trans", "Kenya Cuevas", "Paolita Suárez",
  "Casa de las Muñecas Tiresias", "trabajadoras sexuales trans", "transincluyente", "transexcluyente",
  "trans en prisión", "TERF", "migración trans",
  
  # Lista de palabras
  "trans", "transgénero", "transgéneros", "transexual", "transexualidad", "transexuales", "travesti",
  "vestida", "vestidas", "travestista", "trasvestista", "travestis", "transvesti", "transvestis", "reasignación",
  "autopercibido", "disforia", "transfobia", "transfóbica", "genderqueer", "queer", "magistrade", "binario",
  "identidad", "hormonización", "hormonal", "transincluyente", "transexcluyente", "TERF", "muxe", "LGBT",
  "LGBTTTI", "LGBTI", "LGBTTTIQ", "LGBTTTIQA+", "Drag"
)

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")
muestra <- read_csv("muestra.csv")

# 1. Preparamos corpus
corpus <- muestra$LP

# 2. Creamos función de conteo de ngramas exactos en cada documento
contar_terminos <- function(texto, terminos) {
  sapply(terminos, function(t) str_count(str_to_lower(texto), fixed(str_to_lower(t))))
}

# 3. Aplicamos sobre cada texto del corpus
matriz_conteo <- t(sapply(corpus, contar_terminos, terminos = terminos))
rownames(matriz_conteo) <- if (!is.null(muestra$ID)) muestra$ID else seq_len(nrow(muestra))
colnames(matriz_conteo) <- terminos

# 5. Convertimos a dgCMatrix (formato compatible con TfIdf de text2vec)
dtm <- as(Matrix(matriz_conteo, sparse = TRUE), "dgCMatrix")

# 6. Calcular TF-IDF
tfidf <- TfIdf$new()
tfidf_mat <- tfidf$fit_transform(dtm)

# 7. Guardar la suma TF-IDF por documento en el dataframe original
muestra$tfidf_sum <- Matrix::rowSums(tfidf_mat)


