# csv-filer for testing ligger i mappen:
mappe = "felles/pakker/rapwhale/tests/testthat/"

# Håndtering av inndata fra datadump og spesifikasjon ---------------------
context("les_varnavn")

test_that("Funksjonen leser inn en variabel selv om den ikke har navn", {
  dd_kolonne_uten_navn = "dd_kolonne_uten_navn.csv"
  forventet_uten_navn = c("alfa", "beta", "", "delta", "echo")
  expect_identical(les_kolnavn(dd_sti = paste0(mappe, dd_kolonne_uten_navn)), forventet_uten_navn)
})

test_that("Funksjonen gir feilmelding om to variabler har samme navn i datadump", {
  dd_to_kolonner_med_samme_navn = "dd_to_kolonner_med_samme_navn.csv"
  forventet_med_samme_navn = "Variabler i datadump kan ikke ha samme navn"

  expect_error(les_kolnavn(dd_sti = paste0(mappe, dd_to_kolonner_med_samme_navn)), forventet_med_samme_navn)
})

test_that("Funksjonen leser inn variabler med mellomrom i navnet", {
  dd_kolonnenavn_med_mellomrom = "dd_kolonnenavn_med_mellomrom.csv"
  forventet_med_mellomrom = c("alfa", "beta", "beta bert", "delta", "echo")

  expect_identical(les_kolnavn(dd_sti = paste0(mappe, dd_kolonnenavn_med_mellomrom)), forventet_med_mellomrom)
})

test_that("Funksjonen leser inn variabler hvor navnet starter med et tall", {
  dd_kolonnenavn_som_starter_med_tall = "dd_kolonnenavn_som_starter_med_tall.csv"
  forventet_med_tall = c("alfa", "beta", "2beta", "delta", "echo")

  expect_identical(les_kolnavn(dd_sti = paste0(mappe, dd_kolonnenavn_som_starter_med_tall)), forventet_med_tall)
})

test_that("Funksjonen leser inn variabler med bindestrek i navnet", {
  dd_kolonnenavn_med_bindestrek = "dd_kolonnenavn_med_bindestrek.csv"
  forventet_med_bindestrek = c("alfa", "beta", "beta-bert", "delta", "echo")

  expect_identical(les_kolnavn(dd_sti = paste0(mappe, dd_kolonnenavn_med_bindestrek)), forventet_med_bindestrek)
})
