# datavalideringsskript

# nødvendige pakker
library(tidyverse)
library(ruler) # pakke for validering

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen

d = tribble(
  ~pasid, ~kjonn, ~alder, ~vekt, ~frisk,
  11, 0, 16.23, 30, TRUE,
  12, 1, 22, 50, FALSE,
  13, 1, -14, 60, FALSE,
  14, NA, 80, 70.7, TRUE,
  15, 3, 900, 1000, NA
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

# sjekker at variabler er innfor min verdier
er_innfor_min = row_packs(
  sjekk_min = . %>% transmute(min_alder = alder >= 18, min_vekt = vekt >= 45)
)

# ønsket linjekode: :'(
d %>%
  expose(er_innfor_min) %>%
  get_report() %>%
  left_join((d %>% transmute(id = 1:n(), pasid, alder, vekt)), by = "id") %>%
  print(.validate = FALSE)
