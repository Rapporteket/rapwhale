# Tester for funksjonen lag_regelsett og dd_er_gyldig

# lager fiktivt datasett som inneholder
# en del feil som skal oppdages i valider_datadump-funksjonen
context("Datadump er gyldig")

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

test_that("Feilmelding ved nødvendige variabler som ikke finnes i kodeboka.", {
  kb_mangler_varid = tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst)
  kb_mangler_desimalar_og_varid = tribble(~variabeltype, ~min, ~maks, ~obligatorisk, ~verdi, ~verditekst)

  expect_error(lag_regelsett(kb_mangler_varid),
    regexp = "Kodeboka mangler obligatoriske kolonner: 'variabel_id'."
  )
  expect_error(lag_regelsett(kb_mangler_desimalar_og_varid),
    regexp = "Kodeboka mangler obligatoriske kolonner: 'variabel_id', 'desimalar'."
  )
})

# Test 2 Feilmedling hvis KB mangler verdier for variabeltype eller variabel_id
test_that(paste0(
  "Funksjonen stopper og rapporterer en feilmelding hvis kodeboka mangler ",
  "verdier for variabel_id eller variabel_type."
), {
  kb_feil = tribble(
    ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
    "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
    "alder", NA, 18, NA, TRUE, 0, NA, NA,
    "vekt", "numerisk", 45, 200, TRUE, 0, NA, NA,
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
    NA, "kategorisk", NA, NA, TRUE, NA, 1, "mann",
    "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
  )
  expect_error(lag_regelsett(kb_feil), "Kodeboka har manglende verdier for variabel_id og/eller variabeltype.")
})

# Test 3 Feilmedling hvis KB mangler verdier for variabeltype eller variabel_id
test_that("Funksjonen stopper og rapporterer en feilmelding hvis kategoriske variabler mangler verdi.", {
  kb_kat_feil_verdi = tribble(
    ~variabel_id, ~variabeltype, ~min, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
    "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
    "kjonn", "kategorisk", NA, NA, TRUE, NA, NA, "mann"
  )
  expect_error(lag_regelsett(kb_kat_feil_verdi), "Kategoriske variabler mangler verdier for verdi.")
})

#  Skal være mulig å ha bruke KB på ikke vårt standardformat, med å si at f.eks "min" heter "min_verdi" eller andre.

# nolint start: commented_code_linter.
# test_that("Skal kunne navngi enkeltkolonner i KB.", {
#
#   kb_annet_navn = tribble(
#     ~variabel_id, ~variabeltype, ~min_verdi, ~maks, ~obligatorisk, ~desimalar, ~verdi, ~verditekst,
#     "pasid", "tekst", NA, NA, TRUE, NA, NA, NA,
#     "alder", "numerisk", 18,  NA, TRUE,       0,     NA,         NA,
#     "vekt", "numerisk", 45, 200, TRUE,        0, NA, NA,
#     "kjonn", "kategorisk", NA, NA, TRUE, NA, 0, "kvinne",
#     "kjonn", "kategorisk", NA, NA, TRUE, NA, 1, "mann",
#     "frisk", "boolsk", NA, NA, TRUE, NA, NA, NA
#   )
#   expect_identical(lag_regelsett(kb_annet_navn), lag_regelsett(kb))
# }
# )
# nolint end
