######### Análisis descriptivo

rm(list = ls()) 

install.packages(c("dplyr", "tidytext", "stringr", "text2vec", "tibble", "tokenizers", "purrr"))
library(tidytext)
library(stringr)
library(text2vec)
library(tibble)
library(tokenizers)
library(purrr)
library(Matrix)
library(ggplot2)
library(forcats)
library(tidyr)
library(readr)
library(lubridate)
library(dplyr)
library(stringr)
library(tm)
library(stringi)

# Cargar bases de datos

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")
smos <- read_csv("smos.csv")
muestra <- read_csv("muestra.csv")

smos <- smos %>%
  mutate(
    DT = as.Date(DT),
    anio = year(DT),
    texto = tolower(paste(HD, LP, sep = " ")),
    texto_limpio = texto %>%
      stri_trans_general("Latin-ASCII") %>%          # Quitar tildes y diacríticos
      str_replace_all("[[:punct:]]", " ") %>%        # Eliminar puntuación
      str_replace_all("[[:digit:]]", " ") %>%        # Eliminar números
      str_replace_all("\\s+", " ") %>%               # Espacios múltiples a uno solo
      removeWords(stopwords("spanish")) %>%          # Eliminar stopwords en español
      str_trim()                                     # Quitar espacios al inicio y final
  )

muestra <- muestra %>%
  mutate(
    DT = as.Date(DT),
    anio = year(DT),
    texto = tolower(paste(HD, LP, sep = " ")),
    texto_limpio = texto %>%
      stri_trans_general("Latin-ASCII") %>%          # Quitar tildes y diacríticos
      str_replace_all("[[:punct:]]", " ") %>%        # Eliminar puntuación
      str_replace_all("[[:digit:]]", " ") %>%        # Eliminar números
      str_replace_all("\\s+", " ") %>%               # Espacios múltiples a uno solo
      removeWords(stopwords("spanish")) %>%          # Eliminar stopwords en español
      str_trim()
  )

### SOBRE SMOS

# Listas con tokens, binomios y trinomios
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
  "trans en prisión", "TERF", "migración trans", "diversidad sexual",
  
  # Lista de palabras
  "trans", "transgénero", "transgéneros", "transexual", "transexualidad", "transexuales", "travesti",
  "vestida", "vestidas", "travestista", "trasvestista", "travestis", "transvesti", "transvestis", "reasignación",
  "autopercibido", "disforia", "transfobia", "transfóbica", "genderqueer", "queer", "magistrade", "binario",
  "transincluyente", "transexcluyente", "TERF", "muxe", "LGBT", "LGBT+", "LGBTI", "LGBTI+", "LGBTT", "LGBTT+", 
  "LGBTTT", "LGBTTT+", "LGBTTTI", "LGBTTTI+", "LGBTTTIQ", "LGBTTTIQ+", "LGBTTTIQA", "LGBTTTIQA+", "LGBTQ", 
  "LGBTQ+", "LGBTQI", "LGBTQI+", "LGBTQIA", "LGBTQIA+", "Drag"
)
terminos <- tolower(terminos) %>%
  str_replace_all("[^a-záéíóúüñ\\s]", "") %>%
  str_squish()


###### TF-IDF sobre "SMOS"

# Crear función para extraer solo los términos que aparecen en la lista
extraer_terminos <- function(texto, terminos) {
  encontrados <- str_extract_all(texto, str_c("\\b", terminos, "\\b", collapse = "|"))
  unlist(encontrados)
}

# Aplicar extracción de términos al texto
tokens <- smos %>%
  select(ID, texto) %>%
  rowwise() %>%
  mutate(term = list(extraer_terminos(texto, terminos))) %>%
  unnest(term) %>%
  filter(!is.na(term) & term != "")

# Calcular TF-IDF solo sobre los términos encontrados
tf_idf_df <- tokens %>%
  count(ID, term, sort = TRUE) %>%
  bind_tf_idf(term, ID, n)

# Crear diccionario: términos más relevantes por documento
diccionario <- tf_idf_df %>%
  arrange(desc(tf_idf)) %>%
  group_by(ID) %>%
  top_n(10, tf_idf) %>%
  ungroup()

# Puntuación total TF-IDF por documento
document_scores <- tf_idf_df %>%
  group_by(ID) %>%
  summarise(tfidf_score = sum(tf_idf))

# Añadir puntuación al dataframe original
smos <- smos %>%
  left_join(document_scores, by = "ID")

#Top de términos
top_global_terminos <- diccionario %>%
  group_by(term) %>%
  summarise(score_total = sum(tf_idf)) %>%
  arrange(desc(score_total)) %>%
  slice_max(score_total, n = 15)  # Top 15 términos más importantes

ggplot(top_global_terminos, aes(x = reorder(term, score_total), y = score_total)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Términos más importantes en SMOS",
       x = "Término",
       y = "Puntaje TF-IDF total") +
  theme_minimal()

diccionario <- diccionario %>%
  left_join(smos %>% select(ID, anio), by = "ID")

# Mapa de calor con evolución por año
heatmap_df <- diccionario %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar términos más relevantes globalmente (top 20)
top_terms <- heatmap_df %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%   #Número de terminos
  pull(term)

# Filtrar el dataframe para solo esos términos
heatmap_df <- heatmap_df %>%
  filter(term %in% top_terms)

# Ordenar términos dentro de cada año según score_total (de más alto a más bajo)
heatmap_df <- heatmap_df %>%
  group_by(anio) %>%
  mutate(term_ordenado = fct_reorder(term, score_total, .desc = TRUE)) %>%
  ungroup()

# Graficar heatmap con ejes intercambiados y términos ordenados por año
ggplot(heatmap_df, aes(x = as.factor(anio), y = term_ordenado, fill = score_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Evolution of top trans terms on trans social movements",
    x = "Year",
    y = "Top terms",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### ###### TF-IDF sobre "MUESTRA"

# Crear función para extraer solo los términos que aparecen en la lista
extraer_terminos <- function(texto, terminos) {
  encontrados <- str_extract_all(texto, str_c("\\b", terminos, "\\b", collapse = "|"))
  unlist(encontrados)
}

# Aplicar extracción de términos al texto
tokens_muestra <- muestra %>%
  select(ID, texto) %>%
  rowwise() %>%
  mutate(term = list(extraer_terminos(texto, terminos))) %>%
  unnest(term) %>%
  filter(!is.na(term) & term != "")

# Calcular TF-IDF solo sobre los términos encontrados
tf_idf_df_muestra <- tokens_muestra %>%
  count(ID, term, sort = TRUE) %>%
  bind_tf_idf(term, ID, n)

# Crear diccionario: términos más relevantes por documento
diccionario_muestra <- tf_idf_df_muestra %>%
  arrange(desc(tf_idf)) %>%
  group_by(ID) %>%
  top_n(10, tf_idf) %>%
  ungroup()

# Puntuación total TF-IDF por documento
document_scores_muestra <- tf_idf_df_muestra %>%
  group_by(ID) %>%
  summarise(tfidf_score = sum(tf_idf))

# Añadir puntuación al dataframe original
muestra <- muestra %>%
  left_join(document_scores_muestra, by = "ID")

#Top de términos
top_global_terminos_muestra <- diccionario_muestra %>%
  group_by(term) %>%
  summarise(score_total = sum(tf_idf)) %>%
  arrange(desc(score_total)) %>%
  slice_max(score_total, n = 15)  # Top 15 términos más importantes

ggplot(top_global_terminos_muestra, aes(x = reorder(term, score_total), y = score_total)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Términos más importantes en medios",
       x = "Término",
       y = "Puntaje TF-IDF total") +
  theme_minimal()

diccionario_muestra <- diccionario_muestra %>%
  left_join(smos %>% select(ID, anio), by = "ID")

# Crear dataframe resumido
heatmap_df_muestra <- diccionario_muestra %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar top 20 términos globales
top_terms_muestra <- heatmap_df_muestra %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%   #20 terminos más importantes
  pull(term)

# Filtrar para solo esos términos
heatmap_df_muestra <- heatmap_df_muestra %>%
  filter(term %in% top_terms_muestra)

# Ordenar términos dentro de cada año (aquí no cambia porque va en Y)
heatmap_df_muestra <- heatmap_df_muestra %>%
  group_by(anio) %>%
  mutate(term_ordenado = fct_reorder(term, score_total, .desc = TRUE)) %>%
  ungroup()

# Crear gráfico con ejes intercambiados
ggplot(heatmap_df_muestra, aes(x = as.factor(anio), y = term_ordenado, fill = score_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Evolution of top trans terms on media",
    x = "Year",
    y = "Top term",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Exploración criterio de clasificación de documentos 

quantile(document_scores_muestra$tfidf_score, probs = c(0, 0.25, 0.5, 0.75, 1))

residuales <- muestra %>%
  filter(tfidf_score < 1.6)

medulares <- smos %>%
  filter(tfidf_score > 4)

ggplot(document_scores, aes(y = tfidf_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Distribución de puntajes TF-IDF por documento",
    y = "TF-IDF Score",
    x = ""
  ) +
  theme_minimal()

quantile(document_scores_muestra$tfidf_score, probs = c(0, 0.25, 0.5, 0.75, 1))

ggplot(document_scores_muestra, aes(y = tfidf_score)) +
  geom_boxplot(fill = "skyblue") +
  labs(
    title = "Distribución de puntajes TF-IDF por documento",
    y = "TF-IDF Score",
    x = ""
  ) +
  theme_minimal()


# ¿Sirve TF-IDF para elegir textos relevantes?

###### Semantic Neighbors sobre "SMOS"

if (!require("text2vec")) install.packages("word2vec")
if (!require("tibble")) install.packages("tibble")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(text2vec); library(dplyr); library(stringr); library(tibble)
library(ggraph); library(tidygraph); library(dplyr);library(purrr); library(tidyr)

# Preparar el texto
tokens <- smos$texto_limpio %>%
  tolower() %>%
  str_replace_all("[^\\p{L}\\s]", " ") %>%  # Quitar puntuación
  str_squish() %>%
  word_tokenizer()

# Crear el vocabulario
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5)  # min_count = 5

# Crear el vectorizador y matriz
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Entrenar el modelo word2vec
modelo <- GlobalVectors$new(rank = 400, x_max = 10, learning_rate = 0.05)
modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)

# Sumar vectores de contexto y palabra (opcional pero recomendado)
modelo_matriz <- modelo_fit + t(modelo$components)

# Definir terminos y sus vecinos
terminos <- c("trans", "transgénero", "transgéneros", "transexual", "transexualidad", "transexuales", "travesti",
              "vestida", "vestidas", "travestista", "trasvestista", "travestis", "transvesti", "transvestis", "reasignación",
              "disforia", "transfobia", "transfóbica", "queer", "magistrade", "binario",
              "transincluyente", "transexcluyente", "TERF", "muxe", "LGBT", "LGBT+", "LGBTI", "LGBTI+", "LGBTT", "LGBTT+", 
              "LGBTTT", "LGBTTT+", "LGBTTTI", "LGBTTTI+", "LGBTTTIQ", "LGBTTTIQ+", "LGBTTTIQA", "LGBTTTIQA+", "LGBTQ", 
              "LGBTQ+", "LGBTQI", "LGBTQI+", "LGBTQIA", "LGBTQIA+", "Drag")

vecinos <- lapply(terminos, function(palabra) {
  if (palabra %in% rownames(modelo_matriz)) {
    sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
    vecinos_df <- sort(sim[,1], decreasing = TRUE)[2:9]  # Excluye la palabra en sí misma
    tibble(término = palabra,
           vecino = names(vecinos_df),
           similitud = as.numeric(vecinos_df))
  } else {
    tibble(término = palabra, vecino = NA, similitud = NA)
  }
}) %>% bind_rows()

vecinos_filtrados <- vecinos %>%
  filter(!is.na(vecino) & vecino != "" & !is.na(similitud))

# Convertir a grafo
graph <- as_tbl_graph(vecinos_filtrados)

# Visualizar
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = similitud), color = "gray60") +
  geom_node_point(size = 6, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void() +
  labs(title = "Red Semántica de Términos Trans")

# Evolución de vecinos por año

# Versión larga
vecinos_por_anio <- smos %>%
  filter(!is.na(anio)) %>%
  group_by(anio) %>%
  group_map(~{
    
    # Preparar tokens
    tokens <- .x$texto_limpio %>%
      tolower() %>%
      str_replace_all("[^\\p{L}\\s]", " ") %>%
      str_squish() %>%
      word_tokenizer()
    
    # Crear vocabulario y TCM
    it <- itoken(tokens, progressbar = FALSE)
    vocab <- create_vocabulary(it)
    vocab <- prune_vocabulary(vocab, term_count_min = 5)
    vectorizer <- vocab_vectorizer(vocab)
    tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
    
    # Entrenar modelo GloVe
    modelo <- GlobalVectors$new(rank = 400, x_max = 10, learning_rate = 0.05)
    modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)
    modelo_matriz <- modelo_fit + t(modelo$components)
    
    # Calcular vecinos por término 
    vecinos <- lapply(terminos, function(palabra) {
      if (palabra %in% rownames(modelo_matriz)) {
        sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
        vecinos_df <- sort(sim[,1], decreasing = TRUE)[2:9]  # vecinos más cercanos
        tibble(término = palabra,
               vecino = names(vecinos_df),
               similitud = as.numeric(vecinos_df))
      } else {
        NULL  # No agrega NA, directamente ignora si no existe
      }
    }) %>% bind_rows()
    
    vecinos %>%
      mutate(anio = .y)
  }) %>%
  bind_rows() %>%
  select(anio, término, vecino, similitud)

# Versión ancha
tabla_vecinos_ancha <- smos %>%
  filter(!is.na(anio)) %>%
  group_by(anio) %>%
  group_map(~{
    # Tokenización
    tokens <- .x$texto_limpio %>%
      tolower() %>%
      str_replace_all("[^\\p{L}\\s]", " ") %>%
      str_squish() %>%
      word_tokenizer()
    
    # Vocabulario y matriz TCM
    it <- itoken(tokens, progressbar = FALSE)
    vocab <- create_vocabulary(it)
    vocab <- prune_vocabulary(vocab, term_count_min = 5)
    vectorizer <- vocab_vectorizer(vocab)
    tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
    
    # Entrenamiento del modelo GloVe
    modelo <- GlobalVectors$new(rank = 400, x_max = 10, learning_rate = 0.05)
    modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)
    modelo_matriz <- modelo_fit + t(modelo$components)
    
    # Obtener vecinos por término
    vecinos_lista <- lapply(terminos, function(palabra) {
      if (palabra %in% rownames(modelo_matriz)) {
        sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
        vecinos <- sort(sim[,1], decreasing = TRUE)[2:9]
        paste(names(vecinos), collapse = ", ")
      } else {
        NA_character_
      }
    })
    
    tibble(anio = .y, !!!set_names(vecinos_lista, terminos))
  }) %>%
  bind_rows()

###### Semantic Neighbors sobre "muestra"

if (!require("text2vec")) install.packages("word2vec")
if (!require("tibble")) install.packages("tibble")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(text2vec); library(dplyr); library(stringr); library(tibble)
library(ggraph); library(tidygraph); library(dplyr);library(purrr); library(tidyr)

# Preparar el texto
tokens <- muestra$texto_limpio %>%
  tolower() %>%
  str_replace_all("[^\\p{L}\\s]", " ") %>%  # Quitar puntuación
  str_squish() %>%
  word_tokenizer()

# Crear el vocabulario
it <- itoken(tokens, progressbar = FALSE)
vocab <- create_vocabulary(it)
vocab <- prune_vocabulary(vocab, term_count_min = 5)  # min_count = 5

# Crear el vectorizador y matriz
vectorizer <- vocab_vectorizer(vocab)
tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)

# Entrenar el modelo word2vec
modelo <- GlobalVectors$new(rank = 400, x_max = 10, learning_rate = 0.05)
modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)

# Sumar vectores de contexto y palabra (opcional pero recomendado)
modelo_matriz <- modelo_fit + t(modelo$components)

# Definir terminos y sus vecinos
terminos <- c("trans", "transgénero", "transgéneros", "transexual", "transexualidad", "transexuales", "travesti",
              "vestida", "vestidas", "travestista", "trasvestista", "travestis", "transvesti", "transvestis", "reasignación",
              "disforia", "transfobia", "transfóbica", "queer", "magistrade", "binario",
              "transincluyente", "transexcluyente", "TERF", "muxe", "LGBT", "LGBT+", "LGBTI", "LGBTI+", "LGBTT", "LGBTT+", 
              "LGBTTT", "LGBTTT+", "LGBTTTI", "LGBTTTI+", "LGBTTTIQ", "LGBTTTIQ+", "LGBTTTIQA", "LGBTTTIQA+", "LGBTQ", 
              "LGBTQ+", "LGBTQI", "LGBTQI+", "LGBTQIA", "LGBTQIA+", "Drag")

vecinos <- lapply(terminos, function(palabra) {
  if (palabra %in% rownames(modelo_matriz)) {
    sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
    vecinos_df <- sort(sim[,1], decreasing = TRUE)[2:9]  # Excluye la palabra en sí misma
    tibble(término = palabra,
           vecino = names(vecinos_df),
           similitud = as.numeric(vecinos_df))
  } else {
    tibble(término = palabra, vecino = NA, similitud = NA)
  }
}) %>% bind_rows()

vecinos_filtrados <- vecinos %>%
  filter(!is.na(vecino) & vecino != "" & !is.na(similitud))

# Convertir a grafo
graph <- as_tbl_graph(vecinos_filtrados)

# Visualizar
ggraph(graph, layout = "fr") +
  geom_edge_link(aes(width = similitud), color = "gray60") +
  geom_node_point(size = 6, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 5) +
  theme_void() +
  labs(title = "Red Semántica de Términos Trans")

# Evolución de vecinos por año

# Versión larga
vecinos_por_anio <- muestra %>%
  filter(!is.na(anio)) %>%
  group_by(anio) %>%
  group_map(~{
    
    # Preparar tokens
    tokens <- .x$texto_limpio %>%
      tolower() %>%
      str_replace_all("[^\\p{L}\\s]", " ") %>%
      str_squish() %>%
      word_tokenizer()
    
    # Crear vocabulario y TCM
    it <- itoken(tokens, progressbar = FALSE)
    vocab <- create_vocabulary(it)
    vocab <- prune_vocabulary(vocab, term_count_min = 5)
    vectorizer <- vocab_vectorizer(vocab)
    tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
    
    # Entrenar modelo GloVe
    modelo <- GlobalVectors$new(rank = 200, x_max = 10, learning_rate = 0.05)
    modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)
    modelo_matriz <- modelo_fit + t(modelo$components)
    
    # Calcular vecinos por término 
    vecinos <- lapply(terminos, function(palabra) {
      if (palabra %in% rownames(modelo_matriz)) {
        sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
        vecinos_df <- sort(sim[,1], decreasing = TRUE)[2:9]  # vecinos más cercanos
        tibble(término = palabra,
               vecino = names(vecinos_df),
               similitud = as.numeric(vecinos_df))
      } else {
        NULL  # No agrega NA, directamente ignora si no existe
      }
    }) %>% bind_rows()
    
    vecinos %>%
      mutate(anio = .y)
  }) %>%
  bind_rows() %>%
  select(anio, término, vecino, similitud)


# Versión ancha
tabla_vecinos_ancha <- muestra %>%
  filter(!is.na(anio)) %>%
  group_by(anio) %>%
  group_map(~{
    # Tokenización
    tokens <- .x$texto_limpio %>%
      tolower() %>%
      str_replace_all("[^\\p{L}\\s]", " ") %>%
      str_squish() %>%
      word_tokenizer()
    
    # Vocabulario y matriz TCM
    it <- itoken(tokens, progressbar = FALSE)
    vocab <- create_vocabulary(it)
    vocab <- prune_vocabulary(vocab, term_count_min = 5)
    vectorizer <- vocab_vectorizer(vocab)
    tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
    
    # Entrenamiento del modelo GloVe
    modelo <- GlobalVectors$new(rank = 200, x_max = 10, learning_rate = 0.05)
    modelo_fit <- modelo$fit_transform(tcm, n_iter = 20)
    modelo_matriz <- modelo_fit + t(modelo$components)
    
    # Obtener vecinos por término
    vecinos_lista <- lapply(terminos, function(palabra) {
      if (palabra %in% rownames(modelo_matriz)) {
        sim <- sim2(x = modelo_matriz, y = modelo_matriz[palabra, , drop = FALSE], method = "cosine", norm = "l2")
        vecinos <- sort(sim[,1], decreasing = TRUE)[2:9]
        paste(names(vecinos), collapse = ", ")
      } else {
        NA_character_
      }
    })
    
    tibble(anio = .y, !!!set_names(vecinos_lista, terminos))
  }) %>%
  bind_rows()
