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
  select(symbol, date, close) %>%
  pivot_wider(
    names_from  = symbol,
    values_from = close
  )



close_xts <- xts(
  close_df[-1],
  order.by = close_df$date
)

close_clean <- close_xts[, colSums(is.na(close_xts)) == 0]

returns <- na.omit(diff(log(close_clean)))



