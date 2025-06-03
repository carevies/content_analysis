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


### SOBRE SMOS

#Cargar base de datos
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")
smos <- read_csv("smos.csv")

smos <- smos %>%
  mutate(DT = as.Date(DT), anio = year(DT))

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

smos <- smos %>%
  mutate(texto = paste(HD, LP, sep = " ")) %>%
  mutate(texto = tolower(texto))  # asegurar todo en minúsculas

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

#Cargar base de datos
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")
muestra <- read_csv("muestra.csv")

muestra <- muestra %>%
  mutate(ID = row_number())

muestra <- muestra %>%
  mutate(texto = paste(HD, LP, sep = " ")) %>%
  mutate(texto = tolower(texto))  # asegurar todo en minúsculas

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

### Semantic Neighbors

if (!require("text2vec")) install.packages("word2vec")
if (!require("tibble")) install.packages("tibble")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(text2vec); library(dplyr); library(stringr); library(tibble)

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

library(ggraph)
library(tidygraph)
library(dplyr)

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






#### QUEDA MUY DISPERSO

if (!require(word2vec)) install.packages("word2vec")
if (!require(text2vec)) install.packages("text2vec")
if (!require(Rtsne)) install.packages("Rtsne")
if (!require(dplyr)) install.packages("dplyr")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(stopwords)) install.packages("stopwords")
if (!require(tidyr)) install.packages("tidyr")
if (!require(igraph)) install.packages("igraph")

library(stopwords)
library(stringr)
library(word2vec)
library(text2vec)
library(Rtsne)
library(dplyr)
library(ggplot2)
library(tidyr)
library(igraph)

# Limpieza del texto
stopwords_es <- stopwords("es")

limpiar_texto <- function(texto) {
  texto <- tolower(texto)                      # pasar a minúsculas
  texto <- str_replace_all(texto, "[[:punct:]]", " ")  # eliminar puntuación
  texto <- str_replace_all(texto, "[0-9]", " ")        # eliminar números
  palabras <- unlist(str_split(texto, "\\s+"))          # separar en palabras
  palabras <- palabras[!palabras %in% stopwords_es]     # eliminar stopwords
  palabras <- palabras[palabras != ""]                   # eliminar vacíos
  paste(palabras, collapse = " ")                         # unir de nuevo en texto
}

smos$texto_limpio <- sapply(smos$texto, limpiar_texto)

# Entrenamiento del modelo Word2Vec
modelo_w2v <- word2vec(x = smos$texto, type = "cbow", dim = 100, window = 5, min_count = 150)

# Matriz de vectores semánticos
matriz_vectores <- as.matrix(modelo_w2v)

# Detección de comunidades con k-means
set.seed(123)
n_clusters <- 4  # Número de clusters
clusters <- kmeans(matriz_vectores, centers = n_clusters)

# Asociar palabras con sus comunidades
comunidades <- data.frame(
  palabra = rownames(matriz_vectores),
  comunidad = clusters$cluster
)

# Reducir dimensiones para visualización (t-SNE)
tsne_resultado <- Rtsne(matriz_vectores, dims = 2, perplexity = 30, verbose = TRUE)
tsne_df <- data.frame(
  X = tsne_resultado$Y[,1],
  Y = tsne_resultado$Y[,2],
  palabra = rownames(matriz_vectores),
  comunidad = as.factor(clusters$cluster)
)

# Graficar mapa semántico
ggplot(tsne_df, aes(x = X, y = Y, label = palabra, color = comunidad)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_text(aes(label = palabra), size = 3, vjust = 1, hjust = 1, check_overlap = TRUE) +
  theme_minimal() +
  labs(title = "Mapa semántico de comunidades (Word2Vec + t-SNE)", x = "", y = "")


