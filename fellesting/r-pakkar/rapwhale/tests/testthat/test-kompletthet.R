
context("Erstatt_ukjent")


# Inndata -----------------------------------------------------------------
test_that("Feilmelding hvis data ikke er tibble/data.frame", {})
test_that("Feilmelding hvis na_vektor er tom", {})
test_that("Feilmelding hvis na_vektor og variabel har ulik datatype", {})
test_that("Feilmelding hvis 'variabel' ikke er en streng", {})

# Utdata ------------------------------------------------------------------

test_that("Fungerer uavhengig av hvilken datatype 'variabel' er", {})
test_that("Fungerer med grupperte inndata og ugrupperte inndata", {})
test_that("Konverterer flere verdier hvis det er oppgitt i na_vektor", {})
test_that("Gjør ingenting hvis inndata mangler verdier fra na_vektor", {})
