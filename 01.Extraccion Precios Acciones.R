#Version 0.1 - En desarrollo


library(tidyquant)
library(dplyr)
library(tidyr)
library(xts)



# Listado de acciones


tickers <- c(
  "AGUAS-A.SN","ANDINA-A.SN","ANDINA-B.SN","ANTARCHILE.SN","BCI.SN","BESALCO.SN",
  "NITRATOS.SN","POTASIOS-A.SN","CAP.SN","CMPC.SN","CCU.SN","CENCOSUD.SN",
  "CHILE.SN","ALMENDRAL.SN","CONCHATORO.SN","CAMANCHACA.SN","ITAUCL.SN","COLBUN.SN",
  "COPEC.SN","CRISTALES.SN","ECL.SN","EISA.SN","EMBONOR-B.SN","ENELGXCH.SN",
  "ENELDXCH.SN","ENELAM.SN","ENTEL.SN","ENAEX.SN","FALABELLA.SN","FORUS.SN",
  "HITES.SN","IAM.SN","INGEVEC.SN","ILC.SN","INVERCAP.SN","ABC.SN",
  "MASISA.SN","MULTI-X.SN","NORTEGRAN.SN","ORO-BLANCO.SN","PARAUCO.SN","PAZ.SN",
  "QUINENCO.SN","RIPLEY.SN","SALFACORP.SN","SOCOVESA.SN","SK.SN","SONDA.SN",
  "SQM-B.SN","BSANTANDER.SN","VAPORES.SN","WATTS.SN","BLUMAR.SN","ENELCHILE.SN",
  "TRICOT.SN","SMU.SN","SALMOCAM.SN","MALLPLAZA.SN","CENCOMALLS.SN","MANQUEHUE.SN",
  "CEMENTOS.SN","POLPAICO.SN","HABITAT.SN",
  "PROVIDA.SN","CUPRUM.SN","AFPCAPITAL.SN","EMBONOR-A.SN","ZOFRI.SN","ENJOY.SN",
  "SQM-A.SN","SOQUICOM.SN","AZUL-AZUL.SN","COLO-COLO.SN","CRUZADOS.SN",
  "LTM.SN","BANVIDA.SN","CALICHERAA.SN","CALICHERAB.SN","CAROZZI.SN","CGE.SN",
  "CGET.SN","CTC.SN","COPEVAL.SN","ELECMETAL.SN","HIPERMARC.SN","IANSA.SN",
  "ENLASA.SN","FOSFOROS.SN","INDISA.SN","LAS-CONDES.SN","MELON.SN","MARINSA.SN",
  "MOLLER.SN","MINERA.SN","MOLYMET.SN","PLANVITAL.SN","SIEMEL.SN","SCOTIABKCL.SN",
  "PUCOBRE.SN","PASUR.SN","TRICAHUE.SN","VOLCAN.SN","AAISA.SN","CAMPOS.SN",
  "CINTAC.SN","DUNCANFOX.SN"
)

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

