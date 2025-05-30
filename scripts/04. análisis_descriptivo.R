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
library(ggplot2)
library(lubridate)
library(forcats)


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
terminos <- tolower(terminos)




######Prueba sobre "muestra"

# Preparar corpus
corpus <- smos$LP

# Crear función de conteo de ngramas exactos en cada documento
contar_terminos <- function(texto, terminos) {
  sapply(terminos, function(t) str_count(str_to_lower(texto), fixed(str_to_lower(t))))
}

# Aplicar sobre cada texto del corpus
matriz_conteo <- t(sapply(corpus, contar_terminos, terminos = terminos))
rownames(matriz_conteo) <- if (!is.null(smos$ID)) smos$ID else seq_len(nrow(smos))
colnames(matriz_conteo) <- terminos

# Convertir a dataframe
dtm <- as(Matrix(matriz_conteo, sparse = TRUE), "dgCMatrix")


# Calcular TF-IDF con text2vec
tfidf <- TfIdf$new()
tfidf_mat <- tfidf$fit_transform(dtm)

# Sumar TF-IDF por documento
muestra$tfidf_sum <- Matrix::rowSums(tfidf_mat)





###### Prueba de TF-IDF sobre "smos"

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
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Términos más importantes",
       x = "Término",
       y = "Puntaje TF-IDF total") +
  theme_minimal()

diccionario <- diccionario %>%
  left_join(smos %>% select(ID, anio), by = "ID")

#Top anuales
top_anuales <- diccionario %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop") %>%
  group_by(anio) %>%
  slice_max(score_total, n = 10)

ggplot(top_anuales, aes(x = reorder_within(term, score_total, anio), y = score_total)) +
  geom_col(fill = "salmon") +
  coord_flip() +
  facet_wrap(~ anio, scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Términos más importantes por año",
       x = "Término",
       y = "TF-IDF") +
  theme_minimal()



# Crear el dataframe resumido (ya con años)
heatmap_df <- diccionario %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar términos más relevantes globalmente (top 20)
top_terms <- heatmap_df %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%
  pull(term)

# Filtrar el dataframe para solo esos términos
heatmap_df <- heatmap_df %>%
  filter(term %in% top_terms)

# Graficar heatmap
ggplot(heatmap_df, aes(x = as.factor(anio), y = term, fill = score_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Importancia de términos del movimentos trans por año (TF-IDF)",
    x = "Año",
    y = "Término",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Ordenado 

heatmap_df <- diccionario %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar términos más relevantes globalmente (top 20)
top_terms <- heatmap_df %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%
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
    y = "top terms",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#### SOBRE MUESTRA

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
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Términos más importantes",
       x = "Término",
       y = "Puntaje TF-IDF total") +
  theme_minimal()

diccionario_muestra <- diccionario_muestra %>%
  left_join(smos %>% select(ID, anio), by = "ID")

#Top anuales
top_anuales_muestra <- diccionario_muestra %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop") %>%
  group_by(anio) %>%
  slice_max(score_total, n = 10)

ggplot(top_anuales, aes(x = reorder_within(term, score_total, anio), y = score_total)) +
  geom_col(fill = "salmon") +
  coord_flip() +
  facet_wrap(~ anio, scales = "free_y") +
  scale_x_reordered() +
  labs(title = "Términos más importantes por año",
       x = "Término",
       y = "TF-IDF") +
  theme_minimal()



# Crear el dataframe resumido (ya con años)
heatmap_df_muestra <- diccionario_muestra %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar términos más relevantes globalmente (top 20)
top_terms_muestra <- heatmap_df_muestra %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%
  pull(term)

# Filtrar el dataframe para solo esos términos
heatmap_df_muestra <- heatmap_df_muestra %>%
  filter(term %in% top_terms_muestra)

# Graficar heatmap
ggplot(heatmap_df_muestra, aes(x = as.factor(anio), y = term, fill = score_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Importancia de términos trans en medios por año (TF-IDF)",
    x = "Año",
    y = "Término",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# REORDENAR los términos por importancia total
heatmap_df_muestra <- heatmap_df_muestra %>%
  mutate(term = fct_reorder(term, score_total, .fun = sum, .desc = TRUE))

# Graficar heatmap ordenado
ggplot(heatmap_df_muestra, aes(x = as.factor(anio), y = term, fill = score_total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "darkred") +
  labs(
    title = "Importancia de términos trans en medios por año (TF-IDF)",
    x = "Año",
    y = "Término (ordenado por relevancia)",
    fill = "TF-IDF"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))










# Crear dataframe resumido
heatmap_df_muestra <- diccionario_muestra %>%
  group_by(anio, term) %>%
  summarise(score_total = sum(tf_idf), .groups = "drop")

# Filtrar top 20 términos globales
top_terms_muestra <- heatmap_df_muestra %>%
  group_by(term) %>%
  summarise(score = sum(score_total)) %>%
  slice_max(score, n = 20) %>%
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