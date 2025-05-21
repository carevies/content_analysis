######### Convertir base de datos de Factiva en CVS

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