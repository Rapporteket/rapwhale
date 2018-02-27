# Nødvendige pakkar
library(tibble)
library(dplyr)
library(magrittr)
library(testthat) # for tester

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
  ~varabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
  "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
  "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
  "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
  "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
)

# Tester for lag_regelsett()

# Test 1 Feilmelding hvis testen ikke finner nødvendige kolonner i KB

test_that("Feilmelding ved nødvendige variabler som ikke finnes i kodeboka.", {
  kb_mangler_varid = tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst)
  kb_mangler_desimalar_og_varid = tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~verdi, ~verditekst)

  expect_error(lag_regelsett(kb_mangler_varid), "Kodeboka mangler nødvendige kolonner: 'variabel_id'.")
  expect_error(lag_regelsett(kb_mangler_desimalar_og_varid), "Kodeboka mangler nødvendige kolonner: 'variabel_id', 'desimalar'.")
})

# Test 2 Skal være mulig å ha bruke KB på ikke vårt standardformat, med å si at f.eks "min" heter "min_verdi" eller andre.

test_that("Skal kunne navngi enkeltkolonner i KB.", {
  kb_annet_navn = tribble(
    ~varabel_id, ~variabeltype, ~min_verdi, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
    "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
    "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
    "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
    "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
  )
  expect_identical(lag_regelsett(kb_annet_navn), lag_regelsett(kb))
})
