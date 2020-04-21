
# Kodebok-eksempel og tom kodebok for bruk i tester
{
  # Lager en tom kodebok for å lettere kunne bygge kb for ulike tester.
  kb_tom = tibble(
    skjemanavn = character(),
    navn_i_rapporteket = character(),
    ledetekst = character(),
    obligatorisk = character(),
    type = character(),
    listeverdier = character(),
    listetekst = character(),
    normalintervall_start_numerisk = numeric(),
    normalintervall_slutt_numerisk = numeric(),
    maksintervall_start_numerisk = numeric(),
    maksintervall_slutt_numerisk = numeric(),
    normalintervall_start_dato = as.Date(character()),
    normalintervall_slutt_dato = as.Date(character()),
    maksintervall_start_dato = as.Date(character()),
    maksintervall_slutt_dato = as.Date(character()),
    antall_tegn = integer(),
    lovlige_tegn = character(),
    desimaler = integer(),
    aktiveringsspoersmaal = character(),
    underspoersmaal = character(),
    innfoert_dato = as.Date(character()),
    utfaset_dato = as.Date(character()),
    tabell = character(),
    fysisk_feltnavn = character(),
    kommentar = character(),
    variabel_id = character(),
    hjelpetekst = character()
  )

  # kb_eksempel er de fire første linjene fra AblaNor kodebok.
  # Den inkluderer de kolonnene vi ønsker ut, og riktig kolonnetype for alle kolonner.
  kb_eksempel = data.frame(
    stringsAsFactors = FALSE,
    skjemanavn = c("Personopplysninger", "Personopplysninger", "Personopplysninger", "Basisskjema"),
    navn_i_rapporteket = c(NA_character_, NA_character_, NA_character_, NA_character_),
    ledetekst = c("PID", "Dato for innhenting av opplysninger", "Fødselsnummer", "Årstall for debut arytmi"),
    obligatorisk = c("Ja", "Ja", "Ja", "Nei"),
    type = c("Tekstvariabel", "Datovariabel", "Tekstvariabel", "Tallvariabel"),
    listeverdier = c(NA_character_, NA_character_, NA_character_, NA_character_),
    listetekst = c(NA_character_, NA_character_, NA_character_, NA_character_),
    normalintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_, "birthYear"),
    normalintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_, "todayYear"),
    maksintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_, "birthYear"),
    maksintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_, "todayYear"),
    normalintervall_start_dato = c(NA_character_, NA_character_, NA_character_, NA_character_),
    normalintervall_slutt_dato = c(NA_character_, NA_character_, NA_character_, NA_character_),
    maksintervall_start_dato = c(NA_character_, "'1980-01-01'", NA_character_, NA_character_),
    maksintervall_slutt_dato = c(NA_character_, "today", NA_character_, NA_character_),
    antall_tegn = c(11L, NA_integer_, 11L, 4L),
    lovlige_tegn = c(
      "1234567890", NA_character_,
      "aAáÁbBcCdDðÐeEéÉfFgGhHiIíÍjJkKlLmMnNoOóÓpPqQrRsStTuUvúÚVwWxXyYýÝzZþÞæÆøØåÅäÄöÖñÑéÉ-ü ÜáÁ´_/,.- + 1234567890ØøÜüß@_-!%:;?",
      NA_character_
    ),
    desimaler = c(NA_integer_, NA_integer_, NA_integer_, 0L),
    aktiveringsspoersmaal = c("Nei", "Nei", "Nei", "Nei"),
    underspoersmaal = c("Nei", "Nei", "Nei", "Nei"),
    innfoert_dato = c("1980-01-01", "1980-01-01", "1980-01-01", "2017-04-25"),
    utfaset_dato = c(NA_character_, NA_character_, NA_character_, NA_character_),
    tabell = c("patient", "patient", "patient", "basereg"),
    fysisk_feltnavn = c("ID", "REGISTERED_DATE", "SSN", "DEBUT_ARYT_AAR"),
    kommentar = c(NA_character_, NA_character_, NA_character_, NA_character_),
    variabel_id = c("PATIENT_ID", "PATIENT_REGISTERED_DATE", "PATIENT_SSN", "BASEREG_DEBUT_ARYT_AAR"),
    hjelpetekst = c(
      "Pasient ID - automatisk løpenummer i databasen.",
      "Skriv inn dato for innhentings tidspunkt for opplysninger. Dato skrives på formatet yyyymmdd. Skriver du inn kun dag (dd), så autfylles nåværende måned og år. Skriver du inn måned og dato (mmdd), så autofylles nåværende år. Eksempel: For dato 23.11.1980 er følgende verdier gyldig: 801123, eller 19801123, eller 1980-11-23", "Skriv inn fødselsnummer, 11 siffer. Følgende format for fødselsnummer: ddmmyyxxxxx", "Angi årstall for debut arytmi"
    )
  )
  kb_eksempel = as_tibble(kb_eksempel)
}
# les_kb_oqr_v2 -----------------------------------------------------------
context("les_kb_oqr_v2")
# les_kb_oqr_base --------------------------------------------------------------
context("les_kb_oqr_base")

test_that("Funksjonen leser inn kodebok og returnerer kolonner med forventet format", {
  expect_equal(les_kb_oqr_base("oqr_kodebok.csv"), kb_eksempel)
})

# konverter_tekst() -----------------------------------------------------
context("konverter_tekst")

test_that("funksjonen håndterer konvertering til desimaltall", {
  tekst_til_tall_a = c("1.2", "1.3")
  tekst_til_tall_b = c("1,2", "1,3")
  tekst_til_tall_resultat = c(1.2, 1.3)

  expect_identical(
    konverter_tekst(tekst_til_tall_a,
      regex = "[-]?\\d{1,}\\.\\d{1,}",
      parse_funksjon = readr::parse_double
    ),
    tekst_til_tall_resultat
  )

  expect_identical(
    konverter_tekst(tekst_til_tall_b,
      regex = "[-]?\\d{1,}\\,\\d{1,}",
      parse_funksjon = parse_double
    ),
    tekst_til_tall_resultat
  )
})

test_that("tekstverdier i en desimalvektor blir konvertert til NA", {
  tekst_til_tall_med_na = c("1.2", "birthYear")
  tekst_til_tall_med_na_komma = c("1,2", "birthYear")
  tekst_til_tall_resultat = c(1.2, NA_real_)

  expect_identical(
    konverter_tekst(tekst_til_tall_med_na,
      regex = "[-]?\\d{1,}\\.\\d{1,}",
      parse_funksjon = parse_double
    ),
    tekst_til_tall_resultat
  )

  expect_identical(
    konverter_tekst(tekst_til_tall_med_na_komma,
      regex = "[-]?\\d{1,}\\,\\d{1,}",
      parse_funksjon = parse_double
    ),
    tekst_til_tall_resultat
  )
})

test_that("funksjonen håndterer konvertering til dato", {
  tekst_til_dato_a = c("2020-01-15", "2014-03-10")
  tekst_til_dato_b = c("01-05-2020", "01-15-2020")
  tekst_til_dato_a_resultat = readr::parse_date(tekst_til_dato_a, format = "%Y-%m-%d")
  tekst_til_dato_b_resultat = readr::parse_date(tekst_til_dato_b, format = "%m-%d-%Y")

  expect_identical(konverter_tekst(tekst_til_dato_a,
    regex = "\\d{4}\\-\\d{2}\\-\\d{2}",
    parse_funksjon = parse_date,
    format = "%Y-%m-%d"
  ), tekst_til_dato_a_resultat)

  expect_identical(konverter_tekst(tekst_til_dato_b,
    regex = "\\d{2}\\-\\d{2}\\-\\d{4}",
    parse_funksjon = parse_date,
    format = "%m-%d-%Y"
  ), tekst_til_dato_b_resultat)
})

test_that("funksjonen gir feilmelding ved feil datoformat", {
  tekst_til_dato_a = c("2020-01-15", "2014-03-10")

  expect_error(konverter_tekst(tekst_til_dato_a,
    regex = "\\d{4}\\-\\d{2\\}-\\d{2}",
    parse_funksjon = parse_date,
    format = "%d-%m-%Y"
  ))
})

test_that("tekstverdier i datovektor blir konvertert til NA", {
  tekst_til_dato_med_na = c("2020-01-15", "birthYear")
  tekst_til_dato_res = c(readr::parse_date(tekst_til_dato_med_na[1],
    format = "%Y-%m-%d"
  ), NA)

  expect_identical(
    konverter_tekst(tekst_til_dato_med_na,
      regex = "\\d{4}\\-\\d{2\\}-\\d{2}",
      parse_funksjon = parse_date,
      format = "%Y-%m-%d"
    ),
    tekst_til_dato_res
  )
})

# kb_oqr_base_til_std -----------------------------------------------------

context("kb_oqr_base_til_std")

# legg_til_variabler_kb ---------------------------------------------------
context("legg_til_variabler_kb")
# valider_kodebok ---------------------------------------------------------
context("valider_kodebok")
# Test at det gis feilmelding hvis det finnes avvik mellom listevariabler på ulike skjema
# test_that("Det gis feilmelding hvis en listevariabel har ulik listetekst på ulike skjema", {
#   kb_avvik = kb_tom %>%
#     add_row(skjemanavn = c("basereg", "basereg", "basereg", "op", "op", "op", "patient", "patient", "patient"),
#             fysisk_feltnavn = c(rep("komplikasjon", 9)),
#             type = c(rep("Listevariabel")),
#             listeverdier = c(1,2,3,1,2,3,1,2,3),
#             listetekst = c("Ja", "Nei", "Ukjent", "Ja", "Nei", "Avvik", "Ja", "Nei", "Ukjent"))
#   expect_error(les_kb_oqr_base("oqr_kodebok_avvik.csv"),
#                "Det finnes 1 avvik for listeverdi mellom skjema: \n Variabel  : komplikasjon\n Listeverdi: 3")
#
# })
