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

# Definir ubicación
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

# Cargar base de datos
muestra <- read.csv("muestra.csv")

#Dejar solo las 10 observaciones iniciales (para pruebas de conexión)
muestra <- muestra[500:510, ]

# Crear nuevo data frame para alacenar las respuestas
resultado_muestra_medios <- muestra
resultado_muestra_medios$score <- NA
resultado_muestra_medios$frase <- NA
resultado_muestra_medios$term <- NA
resultado_muestra_medios$characteristics <- NA

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
  if (is.na(texto) || texto == "") return(list(score = NA, frase = NA, term = NA, characteristics = NA))
  
  body <- list(
    model = "llama-3.3-70b-versatile",
    messages = list(
      list(role = "system", content = "Eres un asistente de análisis de texto"),
      list(role = "user", content = paste0(
        "Analiza el siguiente texto y responde exclusivamente en formato JSON válido en español. NO incluyas explicaciones ni encabezados. Tu única salida debe ser un objeto JSON con los siguientes campos:\n\n",
        "- \"score\": uno de estos cuatro valores: \"nada\", \"poco\", \"bastante\" o \"mucho\" (según cuánto hable sobre personas o movimientos trans);\n",
        "- \"frase\": extrae una frase corta y textual del texto donde se hable directamente sobre personas trans (o pon null si no hay);\n",
        "- \"term\": escribe textualmente el término que se usa para referirse a personas trans (o pon null si no hay);\n",
        "- \"characteristics\": una lista con un máximo de tres características mencionadas sobre las personas trans (o una lista vacía si no hay ninguna);\n\n",
        "Si el texto no menciona nada relacionado con personas trans o el movimiento trans, responde exactamente con esto:\n",
        "{ \"score\": \"nada\", \"frase\": null, \"term\": null, \"characteristics\": [] }\n\n",
        "Ejemplo 1:\n",
        "{ \"score\": \"bastante\", \"frase\": \"acusan que integrantes de la comunidad LGBT+ fueron discriminadas\", \"term\": \"comunidad LGBT+\", \"characteristics\": [\"discriminadas\", \"manifestantes\", \"comunidad\"] }\n\n",
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
        
        if (!is.null(respuesta$score) && respuesta$score == "nada") {
          respuesta$frase <- NA_character_
          respuesta$term <- NA_character_
          respuesta$characteristics <- character(0)  # o NA_character_ si lo prefieres
        }
        
        return(respuesta)
      }, error = function(e) {
        warning(paste("Error al parsear JSON:", e))
        return(list(score = NA_character_, frase = NA_character_, term = NA_character_, characteristics = NA))
      })
      return(result) # <--- ¡Agrega este return!
    }
    intentos <- intentos - 1
    Sys.sleep(2)
  }
  # Si todos los intentos fallan, retorna NA
  return(list(score = NA, frase = NA, term = NA, characteristics = NA))
}

# Procesar fila por fila y guardar resultados inmediatamente
resultados <- map(1:nrow(muestra), function(i) {
  Sys.sleep(2)
  print(paste("Procesando fila", i))
  
  texto <- as.character(muestra$LP[i])  # Puede cambiar si se neceitan múltiples columnas
  respuesta <- enviar_prompt(texto)
  
  # Guardar resultado en el dataframe
  resultado_muestra_medios$score[i] <- respuesta$score
  resultado_muestra_medios$frase[i] <- respuesta$frase
  resultado_muestra_medios$term[i] <- respuesta$term 
  resultado_muestra_medios$characteristics[i] <- if (
    is.null(respuesta$characteristics) || all(is.na(respuesta$characteristics))
  ) {
    NA_character_
  } else {
    paste(respuesta$characteristics, collapse = ", ")
  }
  
  # Guardar resultado parcial en disco (cada fila)
  write.csv(resultado_muestra_medios[1:i, ], "resultado_muestra_medios.csv", row.names = FALSE)
  
  return(respuesta)
})

print(head(resultado_muestra_medios))

#### Falta código para auto-actualización del cvs donde se quedó
#### Falta ajustar la temperatura




