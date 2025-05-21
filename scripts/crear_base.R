############### Organizar la base de datos de medios

library(tidyverse)
library(readxl)
library(lubridate)
library(writexl)
library(stringr)

setwd("/Users/carlosvillalobos/Library/CloudStorage/OneDrive-UniversitatdeBarcelona/Documents/Analisis de contenido/Ensayos/Análisis de medios")

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

write_csv(factiva, "medios.csv") #Guardarlo en csv