# datavalideringsskript

# nødvendige pakker
library(tidyverse)
library(ruler) # pakke for validering

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen

d = tribble(
  ~pasid, ~kjonn, ~alder, ~vekt, ~frisk, ~op_dato,
  11, 0, 16.23, 30, TRUE, 1907 - 01 - 01,
  12, 1, 22, 50, FALSE, 2016 - 01 - 14,
  13, 1, -14, 60, FALSE, 2014 - 04 - 07,
  14, NA, 80, 70.7, TRUE, 2013 - 06 - 08,
  15, 3, 900, 1000, NA, 2018 - 03 - 04
)

# lager en fiktiv kodebok som hører til det fiktive datasettet

kb = tribble(
  ~varid, ~vartype, ~min, ~maks, ~oblig, ~des, ~verdi, ~verditekst,
  "pasid", "numerisk", NA, NA, TRUE, NA, NA, NA,
  "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
  "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
  "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
)
