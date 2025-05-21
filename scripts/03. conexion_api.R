######### Conexión API

rm(list = ls()) 

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
setwd("/Users/carlosvillalobos/Library/CloudStorage/OneDrive-UniversitatdeBarcelona/Documents/Analisis de contenido/Ensayos/Análisis de medios")

# Cargar base de datos
muestra <- read.csv("muestra.csv")

# Crear nuevo data frame para alacenar las respuestas
resultado_muestra <- muestra
resultado_muestra$score <- NA
resultado_muestra$term <- NA
resultado_muestra$characteristics <- NA
resultado_muestra$main_topic <- NA
resultado_muestra$sentiment <- NA

# Crear archivo vacío antes del bucle
write.csv(resultado_muestra[0, ], "resultado_muestra_parcial.csv", row.names = FALSE)


# Configurar API de Groq
api_key <- "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" 
api_key2 <- "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" #Por si las dudas
url <- "https://api.groq.com/openai/v1/chat/completions"

# Crear ambiente para contar tokens
tokens_usados <- new.env()
tokens_usados$total <- 0

###Función para conecar R con Groq mediante API
#Características
# Utiliza modelo deepseek-r1-distill-llama-70b
# Límite de 30 RPM y 1,000 RPD
# Límite de 6,000 TPM y sin límite TPD
# Si sale error 426, espera 60s e intenta de nuevo max. 3 veces
# Si supera los 6,000 tokens espera 60s. 
# Se detiene a los 500,000 tokens
# Almacena resultados en un csv


# Función para enviar cada texto a la API
enviar_prompt <- function(texto) {
  if (is.na(texto) || texto == "") return(list(score = NA, term = NA, characteristics = NA, main_topic = NA, sentiment = NA))
  
  body <- list(
    model = "meta-llama/llama-4-maverick-17b-128e-instruct",
    messages = list(
      list(role = "system", content = "Eres un asistente de análisis de texto"),
      list(role = "user", content = paste0(
        "Lee el siguiente texto y responde exclusivamente en formato JSON en español. NO agregues explicaciones ni encabezados. Si no se menciona nada relacionado con personas trans, responde solo con: {\"score\": 0}\n",
        "Debes devolver los siguientes campos:\n",
        "- score: número (0, 0.25, 0.75 o 1) que indique el grado con el habla de personas o movimiento trans;\n",
        "- term: extrae textualmente el término usado para referirse a personas trans;\n",
        "- characteristics: lista (máximo tres) características mencionadas;\n",
        "- main_topic: dos palabras que resumen el tema principal;\n",
        "- sentiment: positivo, negativo o neutro.\n\n",
        "Ejemplo: {\"score\": 0.75, \"term\": \"trasvestis\", \"characteristics\": [\"discriminadas\", \"resilientes\"], \"main_topic\": \"derechos humanos\", \"sentiment\": \"positivo\"}\n\n",
        "Texto: ", texto
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
        
        if (!is.null(respuesta$score) && respuesta$score == 0) {
          respuesta$term <- NA
          respuesta$characteristics <- NA
          respuesta$main_topic <- NA
          respuesta$sentiment <- NA
        }
        
        return(respuesta)
      }, error = function(e) {
        warning(paste("Error al parsear JSON:", e))
        return(list(score = NA, term = NA, characteristics = NA, main_topic = NA, sentiment = NA))
      })
      
      return(result)
    } else {
      return(list(score = NA, term = NA, characteristics = NA, main_topic = NA, sentiment = NA))
    }
  }
  
  return(list(score = NA, term = NA, characteristics = NA, main_topic = NA, sentiment = NA))
}

# Procesar fila por fila y guardar resultados inmediatamente
resultados <- map(1:nrow(muestra), function(i) {
  Sys.sleep(2)
  print(paste("Procesando fila", i))
  
  texto <- as.character(muestra$LP[i])  # Puedes cambiar a paste(na.omit(...)) si necesitas múltiples columnas
  respuesta <- enviar_prompt(texto)
  
  # Guardar resultado en el dataframe
  resultado_muestra$score[i] <- respuesta$score
  resultado_muestra$term[i] <- respuesta$term
  resultado_muestra$characteristics[i] <- paste(respuesta$characteristics, collapse = ", ")
  resultado_muestra$main_topic[i] <- respuesta$main_topic
  resultado_muestra$sentiment[i] <- respuesta$sentiment
  
  # Guardar resultado parcial en disco (cada fila)
  write.csv(resultado_muestra[1:i, ], "resultado_muestra_parcial.csv", row.names = FALSE)
  
  return(respuesta)
})

print(head(resultado_muestra))






