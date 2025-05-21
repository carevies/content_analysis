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
  mutate(id = ifelse(str_detect(class,"HD"),row_number(), NA),
         class = str_replace_all(class, "[^[A-Z]]", "")) %>% 
  fill(id) %>% 
  pivot_wider(names_from = class, values_from = text) %>%   
  mutate(LP = sapply(LP, toString),
         HD = sapply(HD, toString),
         TD = sapply(TD, toString),
         BY = sapply(BY, toString),
         WC = sapply(WC, toString),
         SN = sapply(SN, toString),
         SC = sapply(SC, toString),
         NS = sapply(NS, toString),
         RE = sapply(RE, toString),
         AN = sapply(AN, toString),
         IN = sapply(IN, toString),
         CO = sapply(CO, toString),
         SE = sapply(SE, toString),
         PD = sapply(PD, toString),
         fref = str_extract(RE, "[^:]+"),
         date = dmy(PD),
         date = ifelse(is.na(date), as.Date(as.numeric(PD), origin = "1899-12-30"), date)) %>% 
  distinct(AN, HD, LP, .keep_all = T)

factiva_limpia <- dat %>%
  drop_na(text) %>%
  fill(class, .direction = "down") %>%
  mutate(
    id = ifelse(str_detect(class, "HD"), row_number(), NA),
    class = str_replace_all(class, "[^[A-Z]]", "")
  ) %>%
  fill(id) %>%
  pivot_wider(
    names_from = class,
    values_from = text,
    values_fn = ~ .x[1]  # tomar el primer valor si hay duplicados
  ) %>%
  mutate(
    LP = paste(LP, TD, sep = " "),              # Junta LP y TD
    DT = dmy(PD),                                # Intenta parsear como día-mes-año
    DT = ifelse(is.na(DT),                      # Si falla, interpreta como fecha numérica Excel
                as.Date(as.numeric(PD), origin = "1899-12-30"),
                DT),
    DT = as.Date(DT)                             # Asegura que sea tipo Date
  ) %>%
  distinct(AN, HD, LP, .keep_all = TRUE) %>%
  select(DT, SN, BY, WC, HD, LP, AN)          # Selecciona solo las columnas deseadas


write_csv(factiva_limpia, "medios.csv") #Guardarlo en csv