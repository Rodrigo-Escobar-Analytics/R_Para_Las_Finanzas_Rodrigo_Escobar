
# PARTE 1 DEL RECOMENDADOR DE CARTERAS:

#ESTA PARTE PERMITE REALIZAR LA EXTRACCION
# Y PREPROCESO DE LA BASE DE LOS PRECIOS, LA IMPORTACION DE LA BASE DE TENENCIA
# CUALQUIER DUDA CONSULTAR AL AUTOR: 
# RODRIGO ESCOBAR LANDAETA | RESCOBARL@FEN.UCHILE.CL | LANDAETA77@GMAIL.COM


library(readr)
library(dplyr)
library(tidyquant)


# CARGA DE ARCHIVO CRM DE TENENCIA

TBL_DATA_SHARES_ACCOUNTS <- read_csv2("FILES/INPUTS/TBL_DATA_SHARES_ACCOUNTS.csv")
TBL_ACCOUNT_MANAGER <- read_csv2("FILES/INPUTS/TBL_ACCOUNT_MANAGER.csv")



# CRUCE TABLA SHARES PARA OBTENER EJECUTIVO ASIGNADO


TBL_DATA_SHARES_ACCOUNTS<-TBL_DATA_SHARES_ACCOUNTS%>%left_join(TBL_ACCOUNT_MANAGER, by=c("ID_CLIENTE"="IDACCOUNT"))





# EXTRACCION DE LOS NEMOTECNICOS DE LA BASE DE LA TENENCIA

tickers <- TBL_DATA_SHARES_ACCOUNTS %>%
  distinct(NEMO) %>%
  pull(NEMO)

# GENERAR EL DATAFRAME CON EL ULTIMO PRECIO AJUSTADO DE CADA UNO DE LOS TICKERS

precios_ultimo <- tq_get(
  tickers,
  get  = "stock.prices",
  from = Sys.Date() - 10
) %>%
  group_by(symbol) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(
    NEMO = symbol,
    PRECIO_ULT = adjusted
  )







# CHECK PARA EVITAR TICKETS VACIOSS


faltantes <- setdiff(tickers, precios_ultimo$NEMO)

# RESET YAHOO FINANCE

Sys.sleep(5)


if (length(faltantes) > 0) {
  
  Sys.sleep(5)
  
  precios_reintento <- tq_get(
    faltantes,
    get  = "stock.prices",
    from = Sys.Date() - 10
  ) %>%
    group_by(symbol) %>%
    slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(
      NEMO = symbol,
      PRECIO_ULT = adjusted
    )
  
  precios_ultimo <- bind_rows(
    precios_ultimo,
    precios_reintento
  )
  
}


setdiff(tickers, precios_ultimo$NEMO)



# GENERAR EL DATAFRAME CON EL SET DE PRECIOS DE LOS ULTIMOS 365 DIAS AJUSTADO DE CADA UNO DE LOS TICKERS

precios_hist <- tq_get(
  tickers,
  get  = "stock.prices",
  from = Sys.Date() - 365
)



tickers_ok <- precios_hist %>%
  distinct(symbol) %>%
  pull(symbol)

tickers_fallidos <- setdiff(tickers, tickers_ok)

tickers_fallidos


if (length(tickers_fallidos) > 0) {
  
  Sys.sleep(5)
  
  precios_hist_2 <- tq_get(
    tickers_fallidos,
    get  = "stock.prices",
    from = Sys.Date() - 365
  )
  
} else {
  precios_hist_2 <- NULL
}


precios_hist <- bind_rows(
  precios_hist,
  precios_hist_2
)



tickers_ok <- precios_hist %>%
  distinct(symbol) %>%
  pull(symbol)

tickers_fallidos <- setdiff(tickers, tickers_ok)


n_distinct(precios_hist$symbol)
length(tickers)


# OBTENER EL DATO DEL TIPO DE CAMBIO USD->CLP PARA ACCIONES EXTRANJERAS

usdclp <- tq_get(
  "USDCLP=X",
  get  = "stock.prices",
  from = Sys.Date() - 10
) %>%
  filter(date == max(date)) %>%
  pull(adjusted)


# PEGAR EL ULTIMO PRECIO A LA TABLA DE TENENCIA TBL_DATA_SHARES_ACCOUNTS

TBL_DATA_SHARES_ACCOUNTS <- TBL_DATA_SHARES_ACCOUNTS %>%
  left_join(precios_ultimo, by = "NEMO")

# TRANSFORMAR DE DOLAR A PESOS LAS ACCIONES EXTRANJERAS

TBL_DATA_SHARES_ACCOUNTS <- TBL_DATA_SHARES_ACCOUNTS %>%
  mutate(
    PRECIO_CLP = if_else(
      CATEGORIA == "EXTRANJERA",
      PRECIO_ULT * usdclp,
      PRECIO_ULT
    )
  )

# CALCULAR EL MONTO TOTAL EN CUSTODIA CON EL ULTIMO PRECIO ENCONTRADO

TBL_DATA_SHARES_ACCOUNTS <- TBL_DATA_SHARES_ACCOUNTS %>%
  mutate(MONTO_CUSTODIA = CANTIDAD_EN_CUSTODIA * PRECIO_CLP)

# VERIFICACION DE TICKERS VACIOS

TBL_DATA_SHARES_ACCOUNTS %>% filter(is.na(PRECIO_ULT)) %>% distinct(NEMO)



total_cuenta <- TBL_DATA_SHARES_ACCOUNTS %>%
  group_by(ID_CLIENTE, ID_CUENTA) %>%
  summarise(
    TOTAL_MONTO_CUSTODIA = sum(MONTO_CUSTODIA, na.rm = TRUE),
    .groups = "drop"
  )


TABLA_PESOS_CUENTA <- TBL_DATA_SHARES_ACCOUNTS %>%
  left_join(total_cuenta, by = c("ID_CLIENTE", "ID_CUENTA")) %>%
  mutate(
    PESO = MONTO_CUSTODIA / TOTAL_MONTO_CUSTODIA
  ) %>%
  select(
    ID_CLIENTE,
    ID_CUENTA,
    NEMO,
    PESO
  )


metricas_accion <- precios_hist %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    col_rename = "RET_DIARIO"
  ) %>%
  summarise(
    RET_PROM_DIARIO = mean(RET_DIARIO, na.rm = TRUE),
    DE_DIARIA       = sd(RET_DIARIO, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(NEMO = symbol)


pesos_metricas <- TABLA_PESOS_CUENTA %>%
  inner_join(metricas_accion, by = "NEMO")


TABLA_INDICADORES_CUENTA <- pesos_metricas %>%
  group_by(ID_CLIENTE, ID_CUENTA) %>%
  summarise(
    RET_PROM_DIARIO = sum(PESO * RET_PROM_DIARIO, na.rm = TRUE),
    
    DE_DIARIA = sqrt(
      sum((PESO^2) * (DE_DIARIA^2), na.rm = TRUE)
    ),
    
    RET_PROM_MENSUAL = RET_PROM_DIARIO * 21,
    DE_MENSUAL       = DE_DIARIA * sqrt(21),
    
    RET_PROM_ANUAL   = RET_PROM_DIARIO * 252,
    DE_ANUAL         = DE_DIARIA * sqrt(252),
    
    .groups = "drop"
  )



TBL_DATA_SHARES_ACCOUNTS_FINAL <- TBL_DATA_SHARES_ACCOUNTS %>%
  left_join(
    TABLA_INDICADORES_CUENTA,
    by = c("ID_CLIENTE", "ID_CUENTA")
  )

TBL_DATA_SHARES_ACCOUNTS_FINAL <- TBL_DATA_SHARES_ACCOUNTS_FINAL %>%
  left_join(
    TABLA_PESOS_CUENTA,
    by = c("ID_CLIENTE", "ID_CUENTA","NEMO")
  )



write.table(TBL_DATA_SHARES_ACCOUNTS_FINAL, file = "TBL_DATA_SHARES_ACCOUNTS_FINAL.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)




prices <- tq_get(
  x = tickers,
  from = "2024-01-01",
  get = "stock.prices"
)



close_df <- prices %>%
  select(symbol, date, adjusted) %>%
  pivot_wider(
    names_from  = symbol,
    values_from = adjusted
  )



close_xts <- xts(
  close_df[-1],
  order.by = close_df$date
)

close_clean <- close_xts[, colSums(is.na(close_xts)) == 0]

returns <- na.omit(diff(log(close_clean)))





avg_volume <- prices %>%
  group_by(symbol) %>%
  summarise(
    avg_volume = mean(volume, na.rm = TRUE),
    median_volume = median(volume, na.rm = TRUE)
  )


presence_days <- prices %>%
  group_by(symbol) %>%
  summarise(
    trading_days = sum(volume > 0, na.rm = TRUE),
    total_days   = n(),
    presence_ratio = trading_days / total_days
  )


turnover <- prices %>%
  mutate(turnover = close * volume) %>%
  group_by(symbol) %>%
  summarise(
    avg_turnover = mean(turnover, na.rm = TRUE)
  )

relative_volume <- prices %>%
  group_by(symbol) %>%
  summarise(avg_volume = mean(volume, na.rm = TRUE)) %>%
  mutate(rel_volume = avg_volume / median(avg_volume, na.rm = TRUE))


liquidity_panel <- avg_volume %>%
  left_join(presence_days, by = "symbol") %>%
  left_join(turnover, by = "symbol")


liquidity_panel<-liquidity_panel %>%
  filter(
    presence_ratio > 0.6,
    avg_turnover > quantile(avg_turnover, 0.3, na.rm = TRUE)
  )






write.table(avg_volume, file = "TICKETS.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)


write.table(close_df, file = "close_df.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)






write.table(TBL_Q_TICKERS_EXTRANJERA, file = "TBL_Q_TICKERS_EXTRANJERA.CSV", sep = ";",
            na = "", dec = ",", row.names = FALSE,
            col.names = TRUE)


