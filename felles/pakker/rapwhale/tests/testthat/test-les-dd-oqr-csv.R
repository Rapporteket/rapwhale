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

context("sjekk_varnavn")

dd_ok = "dd_ok.csv"
varnavn_ok = c("alfa", "beta", "cappa", "delta", "echo")
varnavn_feil_rekkefolge = c("alfa", "beta", "delta", "cappa", "echo")
varnavn_feil_navn = c("alfa", "beta", "cappa", "delta", "eple")

# Feilmelding hvis det ikke er overenstemmelse
test_that("Funksjonen gir feilmelding hvis variabelnavn i datadump ikke stemmer overens med navn i spesifikasjon", {
  expect_identical(sjekk_varnavn(paste0(mappe, dd_ok), varnavn_ok), NULL) # Forventer null return
  expect_warning(sjekk_varnavn(paste0(mappe, dd_ok), varnavn_feil_rekkefolge), "Variabelnavn har ulik rekkefølge i datadump og spesifikasjon") # Ulik rekkefølge i spes sammenlignet med dd.
  expect_error(sjekk_varnavn(paste0(mappe, dd_ok), varnavn_feil_navn), "Variabelnavn i spesifikasjon stemmer ikke overens med variabelnavn i datadump") # Feil navn
})


# Konvertering av variabeltyper -------------------------------------------
context("konverter_boolske")

d_base = les_oqr_csv_base(
  dd_sti = paste0(mappe, dd_ok_hel),
  varnavn_kilde = oqr_specs_dd_ok_hel$navn_kilde,
  nye_varnavn = nye_navn_mangler,
  vartype = oqr_specs_dd_ok_hel$vartype,
  csv_bokstav = oqr_specs_dd_ok_hel$csv_bokstav
) # Fixme - Flytte denne opp

d_base_m_feil_logisk = d_base
d_base_m_feil_logisk$epsilon[2] = "2"

test_that("Funksjonen gir feilmelding hvis en boolsk variabel inneholder andre verdier enn (0,1,NA)", {
  expect_error(konverter_boolske(d = d_base_m_feil_logisk, vartype = oqr_specs_dd_ok_hel$vartype),
    "Det finnes ugyldige verdier for en boolsk variabel. (Må være 0,1 eller NA)",
    fixed = TRUE
  )
})

test_that("Funksjonen klarer å konvertere flere boolske", {
  d_base_m_flere_logisk = d_base
  ekstra_logisk = d_base$epsilon
  d_base_m_flere_logisk = bind_cols(d_base_m_flere_logisk, ekstra_logisk = ekstra_logisk)

  forventet_m_ekstra_logisk = d_base_m_flere_logisk
  forventet_m_ekstra_logisk$epsilon = as.logical(d_base_m_flere_logisk$epsilon)
  forventet_m_ekstra_logisk$epsilon = c(TRUE, FALSE, NA, TRUE)
  forventet_m_ekstra_logisk$ekstra_logisk = as.logical(d_base_m_flere_logisk$ekstra_logisk)
  forventet_m_ekstra_logisk$ekstra_logisk = c(TRUE, FALSE, NA, TRUE)

  expect_equal(konverter_boolske(d = d_base_m_flere_logisk, vartype = c(oqr_specs_dd_ok_hel$vartype, "boolsk")), forventet_m_ekstra_logisk)
})
