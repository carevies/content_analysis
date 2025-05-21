######### Conexión API

rm(list = ls()) 

#Instalar packages si fuera necesario
if(!require("httr")) install.packages("httr") & require("httr")
if(!require("jsonlite")) install.packages("jsonlite") & require("jsonlite")
if(!require("dplyr")) install.packages("dplyr") & require("dplyr")
if(!require("purrr")) install.packages("purrr") & require("purrr")
if(!require("tidyverse")) install.packages("tidyverse") & require("tidyverse")


# Cargar paquetes
library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(tidyverse)

# Definir ubicación
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

# Cargar base de datos
muestra <- read.csv("muestra.csv")

#Dejar solo las 10 observaciones iniciales (para pruebas de conexión)
muestra <- muestra[500:510, ]

# Crear nuevo data frame para alacenar las respuestas
resultado_muestra_medios <- data.frame(
  score = rep(NA_integer_, nrow(muestra)),
  term = I(vector("list", nrow(muestra)))
)

# Crear archivo vacío antes del bucle
write.csv(resultado_muestra_medios[0, ], "resultado_muestra_medios.csv", row.names = FALSE)

# Configurar API de Groq
api_key <- "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" 
api_key2 <- "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" #Por si las dudas
url <- "https://api.groq.com/openai/v1/chat/completions"

# Crear ambiente para contar tokens
tokens_usados <- new.env()
tokens_usados$total <- 0

###Función para conecar R con Groq mediante API
#Identificar terminos "trans"

#Características
# Utiliza modelo llama-3.3-70b-versatile
# Límite de 30 RPM y 1,000 RPD
# Límite de 6,000 TPM y sin límite TPD
# Si sale error 426, espera 60s e intenta de nuevo max. 3 veces
# Si supera los 6,000 tokens espera 60s. 
# Se detiene a los 500,000 tokens
# Almacena resultados en un csv


# Función para enviar cada texto a la API
enviar_prompt <- function(texto) {
  if (is.na(texto) || texto == "") return(list(score = NA_integer_, term = NA))
  
  body <- list(
    model = "llama-3.3-70b-versatile",
    messages = list(
      list(role = "system", content = "Eres un asistente de análisis de texto"),
      list(role = "user", content = paste0(
        "Analiza el siguiente texto y responde solo con un objeto JSON válido en español, sin explicaciones ni encabezados.\n",
        "Debe tener dos campos:\n",
        "- \"score\": número (0 o 1) que indique si el texto habla (1) o no (0) sobre personas o movimiento trans;\n",
        "- \"term\": lista con los términos textuales usados para referirse a personas trans (o lista vacía si no hay).\n\n",
        "Ejemplo:\n",
        "{ \"score\": 1, \"term\": [\"personas trans\", \"comunidad LGBT+\"] }\n\n",
        "Texto a analizar:\n", texto
      ))
    ),
    max_tokens = 999
  )
  
  intentos <- 3
  while (intentos > 0) {
    response <- POST(
      url,
      add_headers(Authorization = paste("Bearer", api_key), `Content-Type` = "application/json"),
      body = toJSON(body, auto_unbox = TRUE),
      encode = "json"
    )
    
    print(paste("Código de respuesta:", status_code(response)))
    
    if (tokens_usados$total > 6000) {
      print("Límite de tokens por minuto alcanzado. Esperando 60 segundos...")
      Sys.sleep(60)
      tokens_usados$total <- 0
    }
    
    if (status_code(response) == 429) {
      Sys.sleep(60)
      intentos <- intentos - 1
      next
    }
    
    content_response <- content(response, as = "parsed", encoding = "UTF-8")
    
    if (is.list(content_response) && "choices" %in% names(content_response)) {
      tokens_usados$total <- tokens_usados$total + content_response$usage$total_tokens
      raw_text <- content_response$choices[[1]]$message$content
      
      result <- tryCatch({
        respuesta <- fromJSON(raw_text)
        # score debe ser numérico (0 o 1), si viene como string conviértelo:
        if (!is.null(respuesta$score)) {
          respuesta$score <- as.integer(respuesta$score)
        } else {
          respuesta$score <- NA_integer_
        }
        # Si term es NULL, pon character(0)
        if (is.null(respuesta$term)) {
          respuesta$term <- character(0)
        }
        # No modificar term según score
        return(list(score = respuesta$score, term = respuesta$term))
      }, error = function(e) {
        warning(paste("Error al parsear JSON:", e))
        return(list(score = NA_integer_, term = NA))
      })
      return(result)
    }
    intentos <- intentos - 1
    Sys.sleep(2)
  }
  return(list(score = NA_integer_, term = NA))
}

for (i in seq_len(nrow(muestra))) {
  Sys.sleep(2)
  cat("Procesando fila", i, "\n")
  texto <- as.character(muestra$LP[i])
  respuesta <- enviar_prompt(texto)
  
  resultado_muestra_medios$score[i] <- respuesta$score
  if (is.null(respuesta$term) || all(is.na(respuesta$term))) {
    resultado_muestra_medios$term[[i]] <- NA
  } else {
    resultado_muestra_medios$term[[i]] <- respuesta$term
  }
  
  # Guardar resultado parcial en disco (toda la tabla hasta ahora, SOLO con columnas score y term)
  # Convertir 'term' a string para el CSV
  resultado_muestra_medios$term_txt <- sapply(resultado_muestra_medios$term, function(x) {
    if (is.null(x) || all(is.na(x))) {
      ""
    } else if (length(x) == 0) {
      ""
    } else {
      paste(x, collapse = "; ")
    }
  })
  write.csv(resultado_muestra_medios[1:i, c("score", "term_txt")], "resultado_muestra_medios.csv", row.names = FALSE)
}


#### Falta código para auto-actualización del cvs donde se quedó
#### Falta ajustar la temperatura




