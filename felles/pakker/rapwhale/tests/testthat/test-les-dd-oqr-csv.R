# Håndtering av inndata fra datadump eller spesifikasjon ---------------------
context("les_varnavn")

formatspek_ok_hel = list(
  skilletegn = ";",
  desimaltegn = ",",
  dato = "%d.%m.%Y",
  klokkeslett = "%H:%M",
  dato_kl = "%d.%m.%Y %H:%M",
  tidssone = "Europe/Oslo",
  filkoding = "UTF-8-BOM",
  boolsk_sann = 1,
  boolsk_usann = 0
)

test_that("Funksjonen leser inn en variabel selv om den ikke har navn", {
  forventet_uten_navn = c("alfa", "beta", "", "delta", "echo", "")
  expect_identical(les_varnavn(
    adresse = "dd_kolonne_uten_navn.csv",
    formatspek = formatspek_ok_hel
  ), forventet_uten_navn)
})

# std_koltype_til_readr_koltype -------------------------------------------
context("std_koltype_til_readr_koltype")

test_that("Gir riktig bokstavkode for alle mulige (gyldige) variabeltyper", {
  expect_identical(
    std_koltype_til_readr_koltype(
      c("tekst", "desimaltall", "heltall", "boolsk", "kl", "dato", "dato_kl", "tekst")
    ),
    "cdictDcc"
  )
})

test_that("Håndterer tomme inndata korrekt", {
  expect_identical(std_koltype_til_readr_koltype(character()), "")
})

test_that("Gir feilmelding ved ugyldige variabeltyper", {
  expect_error(
    std_koltype_til_readr_koltype(c("tekst", "status")),
    "Ukjent variabeltype: 'status'"
  )

  expect_error(
    std_koltype_til_readr_koltype(c("farge", "status")),
    "Ukjent variabeltype: 'farge', 'status'"
  )

  expect_error(
    std_koltype_til_readr_koltype(c("tekst", NA)),
    "Variabeltype må defineres for alle variabler"
  )
})

# les_csv_base ------------------------------------------------------------
context("les_csv_base")

# Forventet resultat av å lese inn dd_ok_hel
dd_ok_hel = tibble::tibble(
  Apple = c("Gruppe_1", "Gruppe_2", "Gruppe_2", "Gruppe_1"),
  Beto = c("dette", "er", "en", "tekst"),
  Gammo = c(0.5, 0.12, 0.73, 1.241),
  Delto = c(1L, 5L, 3L, 2L),
  Eple = as.logical(c(TRUE, FALSE, NA_character_, TRUE)),
  Zeppelin = lubridate::dmy_hm(c("01.01.2020 17:00", "05.10.2010 15:00", "03.02.2015 13:30", "05.05.2005 17:55")),
  Estland = lubridate::dmy("12.05.2014", "13.09.1900", "15.01.2015", "15.10.2020"),
  Theeta = hms::as_hms(c("17:00:00", "14:30:00", "12:00:00", "15:05:00"))
)

dd_ok_hel_na = dd_ok_hel %>%
  tibble::add_row(.before = 3)
dd_ok_hel_full = dd_ok_hel %>%
  mutate(epsilon = c("1", "0", "1", "1"))

# Spesifikasjon
specs_dd_ok_hel = tibble::tribble(
  ~varnavn_kilde, ~varnavn_resultat, ~vartype,
  "alfa", "Apple", "tekst",
  "beta", "Beto", "tekst",
  "gamma", "Gammo", "desimaltall",
  "delta", "Delto", "heltall",
  "epsilon", "Eple", "boolsk",
  "zeta", "Zeppelin", "dato_kl",
  "eta", "Estland", "dato",
  "theta", "Theeta", "kl"
)

# Gir forventet format for ulike variabeltyper.
test_that("Funksjonen leser inn datasett og gir ut forventet format", {
  expect_equal(les_csv_base(
    adresse = "dd_ok_hel.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  ), dd_ok_hel)
})

test_that("Funksjonen håndterer NA for alle variabeltyper", {
  expect_equal(les_csv_base(
    adresse = "dd_ok_hel_na.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  ), dd_ok_hel_na)
})

test_that("Funksjonen gir feilmelding ved ukjent dato-format", {
  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_feil_dato_format.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  )))
})

test_that("Funksjonen gir feilmelding om desimaltegn er '.', ikke ','", {
  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_feil_desimaltegn.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  )))
})

test_that("Funksjonen leser inn faktorer som tekst", {
  expect_equal(les_csv_base(
    adresse = "dd_ok_hel.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  ), dd_ok_hel)
})

# Konvertering av variabeltyper -------------------------------------------
context("konverter_boolske")
