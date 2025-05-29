<<<<<<< HEAD
######### Análisis descriptivo

install.packages(c("dplyr", "tidytext", "stringr", "text2vec", "tibble", "tokenizers", "purrr"))
library(dplyr)
library(tidytext)
library(stringr)
library(text2vec)
library(tibble)
library(tokenizers)
library(purrr)

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

# 1. Preparamos corpus
corpus <- muestra$LP

# 2. Creamos función de conteo de ngramas exactos en cada documento
contar_terminos <- function(texto, terminos) {
  sapply(terminos, function(t) str_count(str_to_lower(texto), fixed(str_to_lower(t))))
}

# 3. Aplicamos sobre cada texto del corpus
matriz_conteo <- t(sapply(corpus, contar_terminos, terminos = terminos))

# 4. Convertimos a data.frame
df_conteo <- as.data.frame(matriz_conteo)
colnames(df_conteo) <- terminos
rownames(df_conteo) <- muestra$ID  # si tienes un ID

# 5. Calculamos TF-IDF con text2vec
tfidf <- TfIdf$new()
tfidf_mat <- tfidf$fit_transform(as.matrix(df_conteo))

# 6. Sumamos TF-IDF por documento
muestra$tfidf_sum <- rowSums(tfidf_mat)
=======
######### Análisis descriptivo
>>>>>>> a50518280dadc3823911ee48dbcf150de034e8c3
