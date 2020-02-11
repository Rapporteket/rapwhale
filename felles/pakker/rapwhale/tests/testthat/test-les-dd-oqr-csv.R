# Håndtering av inndata fra datadump og spesifikasjon ---------------------
context("les_varnavn")

formatspek_ok_hel = list(
  skilletegn = ";",
  desimaltegn = ",",
  dato = "%d.%m.%Y",
  klokkeslett = "%H:%M",
  tidssone = "Europe/Oslo",
  filkoding = "UTF-8-BOM",
  boolsk_sann = 1,
  boolsk_usann = 0
) # kan være boolsk_usann = c(0,NA) også

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
    "cdiltDcc"
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
  Eple = c(TRUE, FALSE, NA_character_, TRUE),
  Zeppelin = c("01.01.2020 17:00", "05.10.2010 15:00", "03.02.2015 13:30", "05.05.2005 17:55"),
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
  )$alfa, dd_ok_hel$alfa) # tekst-faktor
  expect_equal(les_csv_base(
    adresse = "dd_ok_hel_full.csv",
    spesifikasjon = specs_dd_ok_hel,
    formatspek = formatspek_ok_hel
  )$epsilon, dd_ok_hel_full$epsilon) # kun numerisk
})

# Konvertering av variabeltyper -------------------------------------------
context("konverter_boolske")

d_base = les_csv_base(
  adresse = "dd_ok_hel.csv",
  spesifikasjon = specs_dd_ok_hel
)

d_base_m_feil_logisk = d_base
d_base_m_feil_logisk$epsilon[2] = "2"

test_that("Funksjonen gir feilmelding hvis en boolsk variabel inneholder andre verdier enn (0,1,NA)", {
  expect_error(konverter_boolske(d = d_base_m_feil_logisk, vartype = specs_dd_ok_hel$vartype),
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

  expect_equal(konverter_boolske(d = d_base_m_flere_logisk, vartype = c(specs_dd_ok_hel$vartype, "boolsk")), forventet_m_ekstra_logisk)
})
