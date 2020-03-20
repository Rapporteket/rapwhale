# Håndtering av inndata fra datadump eller spesifikasjon ---------------------
context("les_varnavn")

formatspek_ok_hel = list(
  skilletegn = ";",
  desimaltegn = ",",
  dato = "%d.%m.%Y",
  klokkeslett = "%H:%M",
  dato_kl = "%d.%m.%Y %H:%M",
  tidssone = "Europe/Oslo",
  tegnkoding = "UTF-8-BOM",
  boolsk_sann = 1,
  boolsk_usann = 0,
  na_verdier = c("", "null")
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
  Eple = c(TRUE, FALSE, NA, TRUE),
  Zeppelin = lubridate::dmy_hm(c("01.01.2020 17:00", "05.10.2010 15:00", "03.02.2015 13:30", "05.05.2005 17:55")),
  Estland = lubridate::dmy("12.05.2014", "13.09.1900", "15.01.2015", "15.10.2020"),
  Theeta = hms::as_hms(c("17:00:00", "14:30:00", "12:00:00", "15:05:00"))
)

dd_ok_hel_na = dd_ok_hel %>%
  tibble::add_row(.before = 3)

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
  specs_dato = specs_dd_ok_hel[specs_dd_ok_hel$vartype == "dato", ]

  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_feil_dato_format.csv", # "2014-05-12"
    spesifikasjon = specs_dato,
    formatspek = formatspek_ok_hel
  ))) # "05.12.2014"
})

test_that("Funksjonen gir feilmelding om desimaltegn i data ikke tilsvarer formatspek", {
  specs_desimal = specs_dd_ok_hel[specs_dd_ok_hel$vartype == "desimaltall", ]

  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_desimal_punktum.csv",
    spesifikasjon = specs_desimal,
    formatspek = formatspek_ok_hel
  )))

  formatspek_desimal = formatspek_ok_hel
  formatspek_desimal$desimaltegn = "."
  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_desimal_komma.csv",
    spesifikasjon = specs_desimal,
    formatspek = formatspek_desimal
  )))
})

test_that("Funksjonen gir feilmelding hvis format tid i data ikke tilsvarer formatspek", {
  specs_kl = specs_dd_ok_hel[specs_dd_ok_hel$vartype == "kl", ]

  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_tid_inkl_sek.csv", # HH:MM:SS
    spesifikasjon = specs_kl,
    formatspek = formatspek_ok_hel
  ))) # HH:MM

  formatspek_kl = formatspek_ok_hel
  formatspek_kl$klokkeslett = "%H:%M:%S"
  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_tid_hh_mm.csv", # HH:MM
    spesifikasjon = specs_kl,
    formatspek = formaspek_kl
  ))) # HH:MM:SS
})

test_that("Funksjonen tolker ikke NA som manglende verdi med mindre den blir bedt om det", {
  dd_spek = tibble::tibble(varnavn_kilde = "land", varnavn_resultat = "land", vartype = "tekst")
  formatspek_na_som_manglende = formatspek_ok_hel
  formatspek_na_som_manglende$na_verdier = c("null", "NA")

  expect_equal(
    les_csv_base(
      adresse = "dd_na_som_vanlig_verdi.csv",
      spesifikasjon = dd_spek,
      formatspek = formatspek_ok_hel
    )$land,
    c("NO", "SV", "NA", "GB")
  )

  expect_equal(
    les_csv_base(
      adresse = "dd_na_som_vanlig_verdi.csv",
      spesifikasjon = dd_spek,
      formatspek = formatspek_na_som_manglende
    )$land,
    c("NO", "SV", NA, "GB")
  )
})

# Testen klarer å tolke na-verdier:
# readr-funksjonen må klare å ha ingen NA,
# eller for eksempel c(NA, null) eller (null, "")


# Konvertering av variabeltyper -------------------------------------------
context("konverter_boolske")

test_that("Funksjonen gir feilmelding hvis en boolsk variabel inneholder ugyldige verdier", {
  expect_error(konverter_boolske(
    x = c(0, 1, NA, 2),
    boolsk_usann = 0,
    boolsk_sann = 1
  ),
  "Det finnes ugyldige verdier for en boolsk variabel: 2\nMulige verdier er: 0,1,NA",
  fixed = TRUE
  )

  expect_error(konverter_boolske(
    x = c("Tr", "Fa", "True", "False", NA),
    boolsk_usann = "Fa",
    boolsk_sann = "Tr"
  ),
  "Det finnes ugyldige verdier for en boolsk variabel: True,False\nMulige verdier er: Fa,Tr,NA",
  fixed = TRUE
  )
})

test_that("Funksjonen godkjenner diverse former for logiske variabler, 1/0, T/F etc", {
  expect_equal(konverter_boolske(
    x = c(0, 1, 1),
    boolsk_usann = 0,
    boolsk_sann = 1
  ), c(FALSE, TRUE, TRUE))
  expect_equal(
    konverter_boolske(
      x = c("True", "Fa", NA),
      boolsk_usann = c("Fa", NA),
      boolsk_sann = c("True")
    ),
    c(TRUE, FALSE, FALSE)
  )
})

test_that("Funksjonen aksepterer flere typer True og False", {
  expect_equal(
    konverter_boolske(
      x = c("Tr", "Fa", "TRUE", "FA", "flips", NA),
      boolsk_usann = c("Fa", "FA", "flips"),
      boolsk_sann = c("Tr", "TRUE")
    ),
    c(TRUE, FALSE, TRUE, FALSE, FALSE, NA)
  )
})

test_that("Funksjonen gir feilmelding om det er like verdier i boolsk_sann og boolsk_usann", {
  expect_error(
    konverter_boolske(
      x = c(0, 1, NA),
      boolsk_usann = c(0, NA),
      boolsk_sann = c(1, NA)
    ),
    "boolsk_sann og boolsk_usann kan ikke inneholde samme verdier"
  )
})

test_that("Funksjonen gir feilmelding hvis det finnes NA-verdier som ikke er akseptert", {
  expect_error(konverter_boolske(
    x = c(0, 1, NA, "null"),
    boolsk_usann = c(0),
    boolsk_sann = c(1),
    na_verdier = "null"
  ),
  "Det finnes ugyldige verdier for en boolsk variabel: NA\nMulige verdier er: 0,1,null",
  fixed = TRUE
  )
})

test_that("Funksjonen fungerer som forventet med ulike na_verdier", {
  expect_identical(
    konverter_boolske(
      x = c(0, 1, NA, "null"),
      boolsk_usann = c(0),
      boolsk_sann = c(1),
      na_verdier = c(NA, "null")
    ),
    c(FALSE, TRUE, NA, NA)
  )

  expect_identical(
    konverter_boolske(
      x = c("JA", "NEI", "NA", NA),
      boolsk_usann = c("NEI", "NA"),
      boolsk_sann = c("JA"),
      na_verdier = c(NA)
    ),
    c(TRUE, FALSE, FALSE, NA)
  )
})

context("konverter_dato_kl")

test_that("les_csv_base gir feilmelding hvis format på dato_kl i formatspek og data ikke er enige", {
  specs_dato_kl = specs_dd_ok_hel[specs_dd_ok_hel$vartype == "dato_kl", ]

  formatspek_dato_kl = formatspek_ok_hel
  formatspek_dato_kl$dato_kl = "%Y.%m.%d %H:%M"

  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_dato_kl.csv", # format: 2020.01.01 17:00
    spesifikasjon = specs_dato_kl,
    formatspek = formatspek_ok_hel
  ))) # format: 01.01.2020 17:00

  expect_error(suppressWarnings(les_csv_base(
    adresse = "dd_alt_dato_kl.csv", # format: 01.01.2020 17:00
    spesifikasjon = specs_dato_kl,
    formatspek = formatspek_dato_kl
  ))) # format: 2020.01.01 17:00
})

# lag_formatspek ----------------------------------------------------------
context("lag_formatspek")

# fixme: Manglar testar på at funksjonen faktisk *fungerer*, dvs. at han gjev
#        ut det han skal når han får gyldige inndata!
test_that("lag_formatspek() fungerer med riktig inndata", {
  expect_identical()
})

# fixme: Manglar testar på at «tidssone» og «tegnkoding» er tekst.
test_that("lag_formatspek() gir feilmelding om tidssone ikke er tekst", {

})

test_that("lag_formatspek() gir feilmelding om tegnkoding ikke er tekst", {

})

# fixme (i les_csv_base()): Variabelen «tegnkoding» (tidlegare «filkoding»)
#                           vert faktisk ikkje brukt til noko!


# fixme: Ifølgje teksten skal også testa «desimaltegn», men det er ingen testar på det.
#        (readr antar for øvrig at desimalteiknet er punktum eller komma,
#         så ein kan kanskje vera like streng)
test_that("lag_formatspek() gir feilmelding hvis skilletegn ikke er tekst og av lengde 1", {
  expect_error(lag_formatspek(
    skilletegn = ";,",
    desimaltegn = ",",
    dato = "%Y-%m-%d",
    klokkeslett = "%H:%M",
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))

  expect_error(lag_formatspek(
    skilletegn = 1,
    desimaltegn = ",",
    dato = "%Y-%m-%d",
    klokkeslett = "%H:%M",
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))
})
test_that("lag_formatspek() gir feilmelding hvis desimaltegn ikke er '.' eller ','", {
  expect_error(lag_formatspek(
    skilletegn = ";",
    desimaltegn = "-",
    dato = "%Y-%m-%d",
    klokkeslett = "%H:%M",
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))
})

test_that("lag_formatspek() gir feilmelding om dato, klokkeslett eller dato_kl ikke er tekst", {
  expect_error(lag_formatspek(
    skilletegn = ";",
    desimaltegn = ",",
    dato = 19900110,
    klokkeslett = "%H:%M",
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))

  expect_error(lag_formatspek(
    skilletegn = ";",
    desimaltegn = ",",
    dato = "%Y-%m-%d",
    klokkeslett = 0000,
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))

  expect_error(lag_formatspek(
    skilletegn = ";",
    desimaltegn = ",",
    dato = "%Y-%m-%d",
    klokkeslett = "%H:%M",
    dato_kl = 199010101530,
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = 1,
    boolsk_usann = 0,
    na_verdier = ""
  ))
})

test_that("lag_formatspek gir feilmelding hvis det er overlapp mellom boolsk_sann og boolsk_usann", {
  expect_error(lag_formatspek(
    skilletegn = ";",
    desimaltegn = ",",
    dato = "%Y-%m-%d",
    klokkeslett = "%H:%M",
    dato_kl = "%Y-%m-%d %H:%M:%OS",
    tidssone = "Europe/Oslo",
    tegnkoding = "UTF-8-BOM",
    boolsk_sann = c(1, 4),
    boolsk_usann = c(0, 4),
    na_verdier = ""
  ))
})
