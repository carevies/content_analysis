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
  # Ahora colapsa cada lista en un solo string (separado por espacio; puedes poner salto de línea si prefieres)
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

Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("rvest")) install.packages("rvest"); library(rvest)
if(!require("stringr")) install.packages("stringr"); library(stringr)
if(!require("dplyr")) install.packages("dplyr"); library(dplyr)
if(!require("readr")) install.packages("readr"); library(readr)
if(!require("beepr")) install.packages("beepr"); library(beepr)
if(!require("lubridate")) install.packages("lubridate"); library(lubridate)

# Funciones ----
# Negar
`%notin%` <- negate(`%in%`) 

# Scrapping ----
amlo_url <- "https://amlo.presidente.gob.mx/secciones/version-estenografica/"

drop <- read_html(amlo_url) %>% 
  html_nodes("div.pagenavi") %>% 
  html_nodes("a") %>% 
  html_attr("href")

# Extraer número máximo de página de forma segura
max_page <- drop %>%
  str_extract("[0-9]+") %>%
  as.numeric() %>%
  max(na.rm = TRUE)

# DF que contendrá todas las observaciones
final <- data.frame()

# x = número de página y loop
for(x in 1:max_page){
  # urls de las páginas en dónde se guardan las versiones estenográficas
  amlos <- read_html(paste0(amlo_url, "page/", as.character(x),"/"))
  
  # urls de las versiones estenográficas
  urls <- amlos %>% 
    html_nodes("header.entry-header") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # títulos
  titles <- amlos %>% 
    html_nodes("h2.entry-title") %>% 
    html_text()
  
  # fechas
  fechas <- 
    as.Date.character(
      str_replace_all(
        paste0(str_sub(titles,1,6),"20",str_sub(titles,7,8)),
        "\\.","-"
      ), format = "%d-%m-%Y"
    )
  
  
  # base temporal por página
  tempo <- data.frame(
    url = urls,
    título = titles,
    fecha = fechas
  )
  
  bodies <- c()
  # for para extraer títulos y textos
  for(i in tempo$url){
    
    wbpg <- read_html(i)
    
    body <- wbpg %>%
      html_nodes("p") %>%
      html_text()
    one_body <- paste(body, collapse=" ")
    bodies <- append(bodies, one_body)
    
  }
  
  tempo$texto <- bodies
  tempo$loop <- as.character(x)
  
  # se guarda cada loop en final
  final <- bind_rows(final, tempo)
  rm(tempo,urls,fechas,titles,bodies,wbpg,body,one_body)
}