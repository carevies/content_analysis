######### Procesar bases de datos

### MEDIOS. Datos extraídos de factiva. 

rm(list = ls()) 

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(purrr)
library(readr)

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

base <- read_excel("bbdd_factiva.xlsx", col_names = FALSE) #Importar sin nombres de columnas

fl <- list.files(pattern = "bbdd_factiva.xlsx", full.names = T)

dat <- map_df(fl, read_xlsx)      
colnames(dat) = c("class", "text") #Nombrar columnas

factiva <- dat %>% 
  drop_na(text) %>% 
  fill(class, .direction = "down") %>% 
  mutate(
    id = ifelse(str_detect(class, "HD"), row_number(), NA),
    class = str_replace_all(class, "[^[A-Z]]", "")
  ) %>% 
  fill(id) %>% 
  group_by(id) %>% 
  mutate(row = row_number()) %>%
  pivot_wider(
    id_cols = id,
    names_from = class,
    values_from = text,
    values_fn = list  # así cada columna es lista de textos por noticia
  ) %>%
  ungroup() %>%
  # Ahora colapsa cada lista en un solo string (separado por espacio)
  mutate(across(
    c(HD, LP, TD, BY, WC, SN, SC, NS, RE, AN, IN, CO, SE, PD),
    ~map_chr(., ~paste(.x, collapse = " "))
  )) %>%
  mutate(
    fref = str_extract(RE, "[^:]+"),
    date = dmy(PD),
    date = ifelse(is.na(date), as.Date(as.numeric(PD), origin = "1899-12-30"), date)
  ) %>%
  distinct(AN, HD, LP, .keep_all = TRUE)

factiva_limpia <- factiva %>%
  mutate(
    LP = case_when(
      LP != "" & TD != "" ~ paste0(LP, ".", TD),
      LP != "" ~ LP,
      TRUE ~ TD
    ),
    DT = as.Date(date)
  ) %>%
  select(DT, SN, BY, WC, HD, LP, AN)


write_csv(factiva_limpia, "medios.csv") #Guardarlo en csv



### SMOS. 

# Extraer datos de Letra S

library(rvest)
library(httr)
library(stringr)

# URL principal
url_principal <- "https://letraese.org.mx/suplemento/"

# Leer el HTML de la página principal
pagina <- read_html(url_principal)

# Extraer todos los enlaces de los suplementos
enlaces_suplementos <- pagina %>%
  html_nodes(".vc_gitem-post-data-source-post_title a") %>%
  html_attr("href")

# Verificar enlaces
head(enlaces_suplementos) 

# Función para extraer el enlace al PDF desde cada página individual
obtener_pdf <- function(url_suplemento) {
  Sys.sleep(1)  # Para no saturar el servidor
  
  # Leer la página del suplemento
  pagina <- tryCatch(read_html(url_suplemento), error = function(e) return(NA))
  
  if (is.na(pagina)) return(NA)
  
  # Buscar enlaces que terminen en .pdf
  enlace_pdf <- pagina %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("\\.pdf$")
  
  # Regresar el primer link PDF que encuentre (suelen tener solo uno)
  if (length(enlace_pdf) > 0) {
    return(enlace_pdf[1])
  } else {
    return(NA)
  }
}

# Aplicar la función a todos los suplementos
enlaces_pdf <- sapply(enlaces_suplementos, obtener_pdf)

# Verifica los resultados
head(enlaces_pdf)

# Situarse donde se guardarán
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

# Crear carpeta de destino si no existe
if (!dir.exists("PDFs_LetraS")) dir.create("PDFs_LetraS")

# Descargar los PDFs
for (url_pdf in enlaces_pdf) {
  nombre_archivo <- basename(url_pdf)
  ruta_destino <- file.path("PDFs_LetraS", nombre_archivo)
  
  # Descargar solo si no existe
  if (!file.exists(ruta_destino)) {
    tryCatch({
      download.file(url_pdf, destfile = ruta_destino, mode = "wb")
      cat("Descargado:", nombre_archivo, "\n")
    }, error = function(e) {
      cat("Error con:", url_pdf, "\n")
    })
  }
}

# Se identifican 9 suplementos que no se descargaron:
# Suplemento-5-diciembre-1996 (Link manda al 5 de la primera epoca)
# Suplemento-29-diciembre-1998 (Link manda al 28)
# Suplemento-43-febrero-2000 (Sin link en página de históricos)
# Suplemento-52-noviembre-2000 (Sin link en página de históricos)
# Suplemento-72-julio-2000 (Sin link en página de históricos)
# Suplemento-96-julio-2004 (Sin link en página de históricos)
# Suplemento-211-febrero2014 (Error)
# Suplemento-225-abril-2015 (Error)
# Suplemento-327-octubre2023 (Link manda a blog)

# Se corrige manualmente el nombre de 10 archivos para que tengan formato "Suplemento-id-mes-anio.pdf:

# Convertir PDF a dataframe 

# Instalar paquetes si no están instalados
if (!require(pdftools)) install.packages("pdftools")

# Cargar librerías necesarias
library(pdftools)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)






# Metadata

# Directorio de trabajo
setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data/PDFs_LetraS")

# Lista de archivos PDF
archivos <- list.files(pattern = "\\.pdf$", full.names = FALSE)

# Meses en español para detección
meses <- c("enero", "febrero", "marzo", "abril", "mayo", "junio",
           "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre")

# Función para extraer ID y fecha
extraer_id_fecha <- function(nombre_archivo) {
  
  # ID: lo que esté después de 'Suplemento-' y antes del siguiente guión
  id <- str_extract(nombre_archivo, "(?<=Suplemento-)[A-Za-z0-9]+")
  
  # Mes: buscar cualquier palabra que coincida con un mes
  mes <- str_extract(nombre_archivo, str_c(meses, collapse = "|"))
  
  # Año: 4 dígitos
  anio <- str_extract(nombre_archivo, "\\d{4}")
  
  # Si mes y año existen, convertirlos a fecha con día 1
  if (!is.na(mes) & !is.na(anio)) {
    fecha <- dmy(paste("1", mes, anio))
  } else {
    fecha <- NA
  }
  
  return(tibble(nombre_archivo, id, fecha))
}

# Aplicar la función a todos los archivos
df_fechas <- map_dfr(archivos, extraer_id_fecha)

library(tidyverse)
library(stringr)

df_titulos <- tibble(enlace = enlaces_suplementos) %>%
  mutate(
    # Extraer ID: buscamos el grupo que está justo antes del título
    id = enlace %>%
      str_remove("https?://letraese.org.mx/") %>%
      str_remove("^suplemento(?:-letra-s)?-") %>%
      str_extract("^[a-zA-Z0-9]+"),
    
    # Extraer título (lo que sigue después del ID)
    titulo = enlace %>%
      str_remove("https?://letraese.org.mx/") %>%
      str_remove("^suplemento(?:-letra-s)?-[a-zA-Z0-9]+-") %>%
      str_remove("/$") %>%
      str_replace_all("-", " ") %>%
      str_to_lower() %>%
      str_replace("^([a-z])", ~str_to_upper(.x))
  ) %>%
  select(id, titulo, enlace)

# Crear una nueva columna "epoca" en ambos DF
df_fechas <- df_fechas %>%
  mutate(
    epoca = if_else(str_detect(nombre_archivo, "primera-epoca"), "primera", "regular")
  )

df_titulos <- df_titulos %>%
  mutate(
    epoca = if_else(str_detect(enlace, "primera-epoca"), "primera", "regular")
  )

# Ahora fusionamos usando ambas columnas: id + epoca
metadata <- df_fechas %>%
  full_join(df_titulos, by = c("id", "epoca"))




# Extracción bruta UN CAOS

for (archivo in archivos) {
  cat("Procesando:", archivo, "\n")
  texto <- tryCatch(pdf_text(archivo), error = function(e) NULL)
  
  if (!is.null(texto)) {
    texto_completo <- paste(texto, collapse = "\n")
    if (nchar(texto_completo) > 100) {
      info <- extraer_info_archivo(basename(archivo))
      if (!is.null(info)) {
        resultados[[length(resultados) + 1]] <- list(
          id = info$id,
          titulo = NA,
          texto = texto_completo,
          fecha = info$fecha
        )
      }
    }
  }
}

# Convertir a data.frame
df_letra_s <- bind_rows(resultados)





#Extracción (NO FUNCIONA)
library(pdftools)
library(dplyr)
library(stringr)
library(purrr)

archivo <- "Suplemento-145-agosto-2008.pdf"
paginas_pdf <- pdf_data(archivo)
paginas_utiles <- paginas_pdf[-1]

# Función para detectar automáticamente columnas
detectar_columnas <- function(df, umbral = 10) {
  # Agrupa por posición x similar (dentro del umbral)
  xs <- sort(unique(df$x))
  col_starts <- xs[c(TRUE, diff(xs) > umbral)]
  col_ids <- sapply(df$x, function(xval) which.min(abs(col_starts - xval)))
  col_ids
}

texto_paginas <- map(paginas_utiles, function(pagina) {
  pagina$columna <- detectar_columnas(pagina)
  # Ordenar por columna, luego por y (de arriba a abajo), luego x (por si hay subcolumnas)
  pagina <- pagina %>% arrange(columna, y, x)
  
  # Agrupar por columna y unir el texto de cada columna
  texto_por_columna <- pagina %>%
    group_by(columna) %>%
    arrange(y, x, .by_group = TRUE) %>%
    summarise(texto = paste(str_squish(text), collapse = " ")) %>%
    pull(texto)
  
  # Unir las columnas con salto de línea (o como prefieras)
  paste(texto_por_columna, collapse = "\n---\n")
})

texto_completo <- paste(texto_paginas, collapse = "\n\n")

cat(texto_completo)

# Nuevo Dataframe
library(pdftools)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Tu función para detectar columnas
detectar_columnas <- function(df, umbral = 10) {
  xs <- sort(unique(df$x))
  col_starts <- xs[c(TRUE, diff(xs) > umbral)]
  col_ids <- sapply(df$x, function(xval) which.min(abs(col_starts - xval)))
  col_ids
}

# Directorio con los PDFs
directorio <- "C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data/PDFs_LetraS"
archivos <- list.files(directorio, pattern = "\\.pdf$", full.names = TRUE)

# Función para procesar un archivo, devuelve data.frame con nombre_archivo y texto
extraer_texto_pdf <- function(archivo) {
  nombre_archivo <- basename(archivo)
  paginas_pdf <- tryCatch(pdf_data(archivo), error = function(e) NULL)
  if (is.null(paginas_pdf)) return(tibble(nombre_archivo = nombre_archivo, texto = NA_character_))
  paginas_utiles <- paginas_pdf[-1]
  
  texto_paginas <- map_chr(paginas_utiles, function(pagina) {
    pagina$columna <- detectar_columnas(pagina)
    pagina <- pagina %>% arrange(columna, y, x)
    texto_por_columna <- pagina %>%
      group_by(columna) %>%
      arrange(y, x, .by_group = TRUE) %>%
      summarise(texto = paste(str_squish(text), collapse = " ")) %>%
      pull(texto)
    paste(texto_por_columna, collapse = "\n---\n")
  })
  
  texto_completo <- paste(texto_paginas, collapse = "\n\n")
  tibble(nombre_archivo = nombre_archivo, texto = texto_completo)
}

# Procesar todos los archivos
texto_df <- map_dfr(archivos, extraer_texto_pdf)

# Unir con metadata
letra_ese <- left_join(metadata, texto_df, by = "nombre_archivo")

setwd("C:/Users/carlosvillalobos156/OneDrive - Universitat de Barcelona/Documents/Analisis de contenido/content_analysis/data")

write_csv(letra_ese, "letra_ese.csv")




#Extraer de almas cautivas

library(rvest)
library(dplyr)

# URL del blog
url <- "https://almascautivasorg.wordpress.com/blog"

# Leer la página
pagina <- read_html(url)

# Extraer los artículos
articulos <- pagina %>% html_elements("article")

# Extraer fecha, título y contenido por cada artículo
datos <- lapply(articulos, function(art) {
  fecha <- art %>% html_element("time") %>% html_attr("datetime")
  titulo <- art %>% html_element(".entry-title") %>% html_text(trim = TRUE)
  contenido <- art %>% html_element(".entry-content") %>% html_text(trim = TRUE)
  data.frame(fecha = fecha, titulo = titulo, contenido = contenido, stringsAsFactors = FALSE)
})

# Unir en un solo data frame
almas_cautivas <- bind_rows(datos)

