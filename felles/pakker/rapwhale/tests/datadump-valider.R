# Tester for funksjonen lag_regelsett og dd_er_gyldig

#' @importFrom testthat test_that expect_error expect_identical
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr pmap
#' @importFrom tidyr nest
#' @importFrom rlang expr
#' @importFrom ruler cell_packs rules col_packs data_packs get_report expose
#' @import purrr

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen

d = dplyr::tribble(
  ~pasid, ~kjonn, ~alder, ~vekt, ~frisk,
  11, 0, 16.23, 30, TRUE,
  12, 1, 22, 50, FALSE,
  13, 1, -14, 60, FALSE,
  14, NA, 80, 70.7, TRUE,
  15, 3, 900, 1000, NA
)

# lager en fiktiv kodebok som hører til det fiktive datasettet

kb = dplyr::tribble(
  ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
  "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
  "alder", "numerisk", 18, NA, TRUE, 0, NA, NA,
  "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
  "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
  "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
)

# Tester for lag_regelsett()

# Test 1 Feilmelding hvis testen ikke finner nødvendige kolonner i KB

testthat::test_that("Feilmelding ved nødvendige variabler som ikke finnes i kodeboka.", {
  kb_mangler_varid = dplyr::tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst)
  kb_mangler_desimalar_og_varid = dplyr::tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~verdi, ~verditekst)

  testthat::expect_error(lag_regelsett(kb_mangler_varid), "Kodeboka mangler obligatoriske kolonner: 'variabel_id'.")
  testthat::expect_error(lag_regelsett(kb_mangler_desimalar_og_varid), "Kodeboka mangler obligatoriske kolonner: 'variabel_id', 'desimalar'.")
})

# Test 2 Feilmedling hvis KB mangler verdier for variabeltype eller variabel_id
testthat::test_that("Funksjonen stopper og rapporterer en feilmelding hvis kodeboka mangler verdier for variabel_id eller variabel_type.", {
  kb_feil = dplyr::tribble(
    ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
    "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
    "alder", NA, 18, NA, TRUE, 0, NA, NA,
    "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
    NA, "kategorisk", NA, NA, TRUE, NA, 1, "mann",
    "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
  )
  testthat::expect_error(lag_regelsett(kb_feil), "Kodeboka har manglende verdier for variabel_id og/eller variabeltype.")
})

# Test 3 Feilmedling hvis KB mangler verdier for variabeltype eller variabel_id
testthat::test_that("Funksjonen stopper og rapporterer en feilmelding hvis kategoriske variabler mangler verdi.", {
  kb_kat_feil_verdi = dplyr::tribble(
    ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
    "kjonn", "kategorisk", NA, NA, TRUE, NA, NA, "mann"
  )
  testthat::expect_error(lag_regelsett(kb_kat_feil_verdi), "Kategoriske variabler mangler verdier for verdi.")
})

#  Skal være mulig å ha bruke KB på ikke vårt standardformat, med å si at f.eks "min" heter "min_verdi" eller andre.

# testthat::test_that("Skal kunne navngi enkeltkolonner i KB.", {
#
#   kb_annet_navn = dplyr::tribble(
#     ~variabel_id, ~variabeltype, ~min_verdi, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
#     "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
#     "alder", "numerisk", 18,  NA, TRUE,       0,     NA,         NA,
#     "vekt", "numerisk", 45, 200, TRUE,        0, NA, NA,
#     "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
#     "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
#     "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
#   )
#   testthat::expect_identical(lag_regelsett(kb_annet_navn), lag_regelsett(kb))
# }
# )
