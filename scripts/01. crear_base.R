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



### SMOS. Datos extraídos de LetraEse.

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

# Se identifican 9 suplementos que no se descargaron. Se descargan manual:
# Suplemento-5-diciembre-1996 (Link manda al 5 de la primera epoca)
# Suplemento-29-diciembre-1998 (Link manda al 28)
# Suplemento-43-febrero-2000 (Sin link en página de históricos)
# Suplemento-52-noviembre-2000 (Sin link en página de históricos)
# Suplemento-72-julio-2000 (Sin link en página de históricos)
# Suplemento-96-julio-2004 (Sin link en página de históricos)
# Suplemento-211-febrero2014 (Error)
# Suplemento-225-abril-2015 (Error)
# Suplemento-327-octubre2023 (Link manda a blog)



