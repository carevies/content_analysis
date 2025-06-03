######### Análisis descriptivo

rm(list = ls()) 

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
library(ggplot2)
library(lubridate)
library(forcats)
library(tidyr)


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


### INTENTO con Red semántica de coocurrencias (TEXTNET)

if (!require("word2vec")) install.packages("word2vec")
if (!require("text")) install.packages("text")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")
library(stringr); library(dplyr); library(text);library(word2vec)

smos <- smos %>%
  mutate(texto_limpio = texto_limpio %>%
           str_to_lower() %>%  # Minúsculas
           str_replace_all("[^a-záéíóúüñ\\s]", " ") %>%  # Quita signos y símbolos
           str_squish()  # Elimina espacios dobles
  )

# Paso 1: Prepara los textos

# Paso 2: Entrena el modelo Word2Vec
modelo <- word2vec(
  x = smos$texto_limpio,
  type = "skip-gram",         # puedes cambiar a "cbow" si tu corpus es más grande
  dim = 100,                  # 100 dimensiones (en lugar de 400)
  window = 6,                 # ventana de 6
  min_count = 3,              # cuenta mínima de 3 apariciones
  iter = 10,                  # número de épocas
  threads = parallel::detectCores()
)

# Paso 3: Convierte el modelo a matriz para consulta
matriz_vectores <- as.matrix(modelo)

# Paso 4: Consulta los vecinos para cada término de interés
# tu lista de términos debe llamarse "terminos" y estar en minúsculas
# por ejemplo:
# terminos <- c("afeminado", "asexual", "bisexual", "drag", "transfobia", ...)

vecinos <- lapply(terminos, function(t) {
  tryCatch({
    resultado <- predict(modelo, newdata = t, type = "nearest", top_n = 8)
    tibble(termino = t, vecino = resultado$term, similitud = resultado$similarity)
  }, error = function(e) {
    tibble(termino = t, vecino = NA, similitud = NA)
  })
}) %>%
  bind_rows()

# Paso 5 (opcional): Visualiza en formato tabla por término
library(tidyr)

tabla_final <- vecinos %>%
  group_by(termino) %>%
  mutate(rank = row_number()) %>%
  pivot_wider(names_from = rank, values_from = vecino, names_prefix = "vecino_")

# Vista final
print(tabla_final, n = Inf)

