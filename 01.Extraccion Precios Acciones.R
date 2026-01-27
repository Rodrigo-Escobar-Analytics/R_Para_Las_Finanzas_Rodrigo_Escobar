
# PARTE 1 DEL RECOMENDADOR DE CARTERAS:

# ESTA PARTE PERMITE REALIZAR LA EXTRACCION Y PREPROCESO DE LA BASE DE LOS PRECIOS, 
# SEGUN EL DICCIONARIO DE TICKETS DISPONIBILIZADO QUE CONTIENE TODAS LAS TENENCIAS DISPONIBLES EN LAS CARTERAS
# CUALQUIER DUDA CONSULTAR AL AUTOR: 
# RODRIGO ESCOBAR LANDAETA | RESCOBARL@FEN.UCHILE.CL | LANDAETA77@GMAIL.COM


library(readxl)
library(purrr)
library(tidyquant)
library(dplyr)

# CARGA DICCIONARIO

DICCIONARIO_TICKERS <- read_excel("FILES/INPUTS/DICCIONARIO_TICKERS.xlsx")

TICKERS <- DICCIONARIO_TICKERS %>%
  distinct(NEMO) %>%
  pull(NEMO)


source("FUNCTIONS/YFINANCE_BATCH.R")



precios_raw_ult <- tq_get_batch(
  TICKERS = TICKERS,
  from    = Sys.Date() - 10,
  batch_size = 50,
  sleep_sec  = 5
)

precios_ultimo <- precios_raw_ult %>%
  group_by(symbol) %>%
  slice_max(date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    NEMO = symbol,
    PRECIO_ULT = adjusted
  )


DICCIONARIO_TICKERS<-DICCIONARIO_TICKERS%>%left_join(precios_ultimo,by="NEMO")




write.table(DICCIONARIO_TICKERS, file = "FILES/INTERMEDIO/01_PRECIOS_ACCIONES.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)




precios_hist <- tq_get_batch(
  TICKERS = TICKERS,
  from    = Sys.Date() - 365
)

