# tester for les_kb_v2.R


# les_kb_oqr_base --------------------------------------------------------------

test_that("Funksjonen leser inn kodebok og returnerer kolonner med forventet format", {

  # kb_eksempel er de fire første linjene fra AblaNor kodebok.
  # Den inkluderer de kolonnene vi ønsker ut, og riktig kolonnetype for alle kolonner.
  kb_eksempel = data.frame(
    stringsAsFactors = FALSE,
    skjemanavn = c("Personopplysninger", "Personopplysninger", "Personopplysninger"),
    navn_i_rapporteket = c(NA_character_, NA_character_, NA_character_),
    ledetekst = c("PID", "Dato for innhenting av opplysninger", "Fødselsnummer"),
    obligatorisk = c("Ja", "Ja", "Ja"),
    type = c("Tekstvariabel", "Datovariabel", "Tekstvariabel"),
    listeverdier = c(NA_character_, NA_character_, NA_character_),
    listetekst = c(NA_character_, NA_character_, NA_character_),
    normalintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_),
    normalintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_),
    maksintervall_start_numerisk = c(NA_real_, NA_real_, NA_real_),
    maksintervall_slutt_numerisk = c(NA_real_, NA_real_, NA_real_),
    normalintervall_start_dato = as.Date(c(NA, NA, NA), format = "%Y-%m-%d"),
    normalintervall_slutt_dato = as.Date(c(NA, NA, NA), format = "%Y-%m-%d"),
    maksintervall_start_dato = as.Date(c(NA, "1980-01-01", NA), format = "%Y-%m-%d"),
    maksintervall_slutt_dato = as.Date(c(NA, NA, NA), format = "%Y-%m-%d"),
    antall_tegn = c(11L, NA_integer_, 11L),
    lovlige_tegn = c(
      "1234567890", NA_character_,
      "aAáÁbBcCdDðÐeEéÉfFgGhHiIíÍjJkKlLmMnNoOóÓpPqQrRsStTuUvúÚVwWxXyYýÝzZþÞæÆøØåÅäÄöÖñÑéÉ-ü ÜáÁ´_/,.- + 1234567890ØøÜüß@_-!%:;?"
    ),
    desimaler = c(NA_integer_, NA_integer_, NA_integer_),
    aktiveringsspoersmaal = c("Nei", "Nei", "Nei"),
    underspoersmaal = c("Nei", "Nei", "Nei"),
    innfoert_dato = as.Date(c("01.01.1980", "01.01.1980", "01.01.1980"), format = "%d.%m.%Y"),
    utfaset_dato = as.Date(c(NA, NA, NA), format = "%d.%m.%Y"),
    tabell = c("patient", "patient", "patient"),
    fysisk_feltnavn = c("ID", "REGISTERED_DATE", "SSN"),
    kommentar = c(NA_character_, NA_character_, NA_character_),
    variabel_id = c("PATIENT_ID", "PATIENT_REGISTERED_DATE", "PATIENT_SSN"),
    hjelpetekst = c(
      "Pasient ID - automatisk løpenummer i databasen.",
      "Skriv inn dato for innhentings tidspunkt for opplysninger. Dato skrives på formatet yyyymmdd. Skriver du inn kun dag (dd), så autfylles nåværende måned og år. Skriver du inn måned og dato (mmdd), så autofylles nåværende år. Eksempel: For dato 23.11.1980 er følgende verdier gyldig: 801123, eller 19801123, eller 1980-11-23", "Skriv inn fødselsnummer, 11 siffer. Følgende format for fødselsnummer: ddmmyyxxxxx"
    )
  )
  kb_eksempel = as_tibble(kb_eksempel)


  expect_equal(les_kb_oqr_base("oqr_kodebok.csv"), kb_eksempel)
})

# test_that("Datovariabler som inkluderer tekst (eksempel 'today') blir korrekt håndtert", {
# })
#
# test_that("Datovariabler med apostrof rundt dato (eksempel '1900-01-01') blir korrekt tolket", {
# })
#
# test_that("Heltallvariabler som inkluderer tekst (eksempel 'birthyear') blir korrekt håndtert", {
# })

# kb_oqr_base_til_std -----------------------------------------------------


# valider_kodebok ---------------------------------------------------------


# legg_til_variabler_kb ---------------------------------------------------


# les_kb_oqr_v2 -----------------------------------------------------------
