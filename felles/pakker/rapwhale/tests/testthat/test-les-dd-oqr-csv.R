# Håndtering av inndata fra datadump og spesifikasjon ---------------------
context("les_varnavn")

test_that("Funksjonen leser inn en variabel selv om den ikke har navn", {
  forventet_uten_navn = c("alfa", "beta", "", "delta", "echo")
  expect_identical(les_varnavn(dd_sti = "dd_kolonne_uten_navn.csv"), forventet_uten_navn)
})

test_that("Funksjonen gir feilmelding om to variabler har samme navn i datadump", {
  forventet_med_samme_navn = "Variabler i datadump kan ikke ha samme navn"

  expect_error(les_varnavn(dd_sti = "dd_to_kolonner_med_samme_navn.csv"), forventet_med_samme_navn)
})

test_that("Funksjonen leser inn variabler med mellomrom i navnet", {
  forventet_med_mellomrom = c("alfa", "beta", "beta bert", "delta", "echo")

  expect_identical(les_varnavn(dd_sti = "dd_kolonnenavn_med_mellomrom.csv"), forventet_med_mellomrom)
})

test_that("Funksjonen leser inn variabler hvor navnet starter med et tall", {
  forventet_med_tall = c("alfa", "beta", "2beta", "delta", "echo")

  expect_identical(les_varnavn(dd_sti = "dd_kolonnenavn_som_starter_med_tall.csv"), forventet_med_tall)
})

test_that("Funksjonen leser inn variabler med bindestrek i navnet", {
  forventet_med_bindestrek = c("alfa", "beta", "beta-bert", "delta", "echo")

  expect_identical(les_varnavn(dd_sti = "dd_kolonnenavn_med_bindestrek.csv"), forventet_med_bindestrek)
})

context("sjekk_varnavn")

varnavn_ok = c("alfa", "beta", "cappa", "delta", "echo")
varnavn_feil_rekkefolge = c("alfa", "beta", "delta", "cappa", "echo")
varnavn_feil_navn = c("alfa", "beta", "cappa", "delta", "eple")

# Feilmelding hvis det ikke er overenstemmelse
test_that("Funksjonen gir feilmelding hvis variabelnavn i datadump ikke stemmer overens med navn i spesifikasjon", {
  expect_identical(sjekk_varnavn("dd_ok.csv", varnavn_ok), NULL) # Forventer null return
  expect_warning(sjekk_varnavn("dd_ok.csv", varnavn_feil_rekkefolge), "Variabelnavn har ulik rekkefølge i datadump og spesifikasjon") # Ulik rekkefølge i spes sammenlignet med dd.
  expect_error(sjekk_varnavn("dd_ok.csv", varnavn_feil_navn), "Variabelnavn i spesifikasjon stemmer ikke overens med variabelnavn i datadump") # Feil navn
})

# Konvertering av variabeltyper -------------------------------------------
context("konverter_boolske")

oqr_specs_dd_ok_hel = tibble::tribble(
  ~navn_kilde, ~nye_varnavn, ~vartype, ~csv_bokstav,
  "alfa", "Abso", "kategorisk", "c",
  "beta", "Beto", "tekst", "c",
  "gamma", "Gammo", "numerisk", "d",
  "delta", "Delto", "heltall", "i",
  "epsilon", "Eple", "boolsk", "c",
  "zeta", "Zeppelin", "dato_kl", "c",
  "eta", "Estland", "dato", "D",
  "theta", NA_character_, "kl", "t"
)

d_base = les_oqr_csv_base(
  dd_sti = "dd_ok_hel.csv",
  varnavn_kilde = oqr_specs_dd_ok_hel$navn_kilde,
  nye_varnavn = oqr_specs_dd_ok_hel$nye_varnavn,
  vartype = oqr_specs_dd_ok_hel$vartype,
  csv_bokstav = oqr_specs_dd_ok_hel$csv_bokstav
)

d_base_m_feil_logisk = d_base
d_base_m_feil_logisk$Eple[2] = "2"

test_that("Funksjonen gir feilmelding hvis en boolsk variabel inneholder andre verdier enn (0,1,NA)", {
  expect_error(konverter_boolske(d = d_base_m_feil_logisk, vartype = oqr_specs_dd_ok_hel$vartype),
    "Det finnes ugyldige verdier for en boolsk variabel. (Må være 0,1 eller NA)",
    fixed = TRUE
  )
})

test_that("Funksjonen klarer å konvertere flere boolske", {
  d_base_m_flere_logisk = d_base
  ekstra_logisk = d_base$Eple
  d_base_m_flere_logisk = bind_cols(d_base_m_flere_logisk, ekstra_logisk = ekstra_logisk)

  forventet_m_ekstra_logisk = d_base_m_flere_logisk
  forventet_m_ekstra_logisk$Eple = as.logical(d_base_m_flere_logisk$Eple)
  forventet_m_ekstra_logisk$Eple = c(TRUE, FALSE, NA, TRUE)
  forventet_m_ekstra_logisk$ekstra_logisk = as.logical(d_base_m_flere_logisk$ekstra_logisk)
  forventet_m_ekstra_logisk$ekstra_logisk = c(TRUE, FALSE, NA, TRUE)

  expect_equal(konverter_boolske(d = d_base_m_flere_logisk, vartype = c(oqr_specs_dd_ok_hel$vartype, "boolsk")), forventet_m_ekstra_logisk)
})
