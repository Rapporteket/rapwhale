# tester for les_kb_v2.R

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
    normalintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_, NA_real_),
    normalintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_, NA_real_),
    maksintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_, NA_real_),
    maksintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_, NA_real_),
    normalintervall_start_dato = as.Date(c(NA, NA, NA, NA), format = "%Y-%m-%d"),
    normalintervall_slutt_dato = as.Date(c(NA, NA, NA, NA), format = "%Y-%m-%d"),
    maksintervall_start_dato = as.Date(c(NA, "1980-01-01", NA, NA), format = "%Y-%m-%d"),
    maksintervall_slutt_dato = as.Date(c(NA, NA, NA, NA), format = "%Y-%m-%d"),
    antall_tegn = c(11L, NA_integer_, 11L, 4L),
    lovlige_tegn = c(
      "1234567890", NA_character_,
      "aAáÁbBcCdDðÐeEéÉfFgGhHiIíÍjJkKlLmMnNoOóÓpPqQrRsStTuUvúÚVwWxXyYýÝzZþÞæÆøØåÅäÄöÖñÑéÉ-ü ÜáÁ´_/,.- + 1234567890ØøÜüß@_-!%:;?",
      NA_character_
    ),
    desimaler = c(NA_integer_, NA_integer_, NA_integer_, 0L),
    aktiveringsspoersmaal = c("Nei", "Nei", "Nei", "Nei"),
    underspoersmaal = c("Nei", "Nei", "Nei", "Nei"),
    innfoert_dato = as.Date(c("1980-01-01", "1980-01-01", "1980-01-01", "2017-04-25"), format = "%Y-%m-%d"),
    utfaset_dato = as.Date(c(NA, NA, NA, NA), format = "%Y-%m-%d"),
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
# les_kb_oqr_base --------------------------------------------------------------

test_that("Funksjonen leser inn kodebok og returnerer kolonner med forventet format", {

  # I oqr_kodebok finnes det:
  # tallvariabler som inneholder tekst (feks birthYear),
  # datovariabel med ekstra tødler
  # datovariabel med tekst (today)
  # Testen sjekker at disse leses inn og konverteres til ønsket verdi og format
  expect_equal(les_kb_oqr_base("oqr_kodebok.csv"), kb_eksempel)
})

# Test konvertering til desimaltall
# i datafil er maksintervall_slutt_numerisk == c(birthYear, birthYear)
test_that("funksjonen håndterer variabler med desimaltall", {
  kb_desimal = kb_tom %>%
    add_row(
      normalintervall_start_numerisk = c(1.2, 1.3),
      normalintervall_slutt_numerisk = c(1, 5),
      maksintervall_start_numerisk = c(2, 3),
      maksintervall_slutt_numerisk = c(NA_real_, NA_real_)
    )

  expect_equal(les_kb_oqr_base("oqr_kodebok_desimal.csv"), kb_desimal)
})

test_that("funksjonen håndterer feil desimaltegn", {
  kb_desimal = kb_tom %>%
    add_row(
      normalintervall_start_numerisk = c(1.2, 1.3),
      normalintervall_slutt_numerisk = c(1, 5),
      maksintervall_start_numerisk = c(2, 3),
      maksintervall_slutt_numerisk = c(NA_real_, NA_real_)
    )
  expect_equal(les_kb_oqr_base("oqr_kodebok_desimal_feil_format.csv"), kb_desimal)
})


# Test at det gis feilmelding hvis det finnes avvik mellom listevariabler på ulike skjema
test_that("Det gis feilmelding hvis en listevariabel har ulik listetekst på ulike skjema", {
  kb_avvik = kb_tom %>%
    add_row(
      skjemanavn = c("basereg", "basereg", "basereg", "op", "op", "op", "patient", "patient", "patient"),
      fysisk_feltnavn = c(rep("komplikasjon", 9)),
      type = c(rep("Listevariabel")),
      listeverdier = c(1, 2, 3, 1, 2, 3, 1, 2, 3),
      listetekst = c("Ja", "Nei", "Ukjent", "Ja", "Nei", "Avvik", "Ja", "Nei", "Ukjent")
    )
  expect_error(
    les_kb_oqr_base("oqr_kodebok_avvik.csv"),
    "Det finnes 1 avvik for listeverdi mellom skjema: \n Variabel  : komplikasjon\n Listeverdi: 3"
  )
})


# kb_oqr_base_til_std -----------------------------------------------------


# valider_kodebok ---------------------------------------------------------


# legg_til_variabler_kb ---------------------------------------------------


# les_kb_oqr_v2 -----------------------------------------------------------
