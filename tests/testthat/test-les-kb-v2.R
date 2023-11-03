
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

  # Tom kodebok etter konvertering til inklusive ekstra kolonner fra OQR
  kb_tom_mellom = tibble(
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
    hjelpetekst = character(),
    skjema_id = character(),
    skjemanamn = character(),
    variabeletikett = character(),
    forklaring = character(),
    variabeltype = character(),
    verdi = character(),
    verditekst = character(),
    desimalar = integer(),
    min = numeric(),
    maks = numeric(),
    min_rimeleg = numeric(),
    maks_rimeleg = numeric(),
    min_dato = as.Date(character()),
    maks_dato = as.Date(character()),
    min_rimeleg_dato = as.Date(character()),
    maks_rimeleg_dato = as.Date(character()),
    kategori = character(),
    innleiing = character(),
    eining = character(),
    unik = character(),
    manglande = character(),
    kommentar_rimeleg = character(),
    utrekningsformel = character(),
    logikk = character()
  )

  # Tom kodebok etter konvertering til standard navn
  kb_tom_std = tibble(
    skjema_id = character(),
    skjemanavn = character(),
    kategori = character(),
    innleiing = character(),
    variabel_id = character(),
    variabeletikett = character(),
    forklaring = character(),
    variabeltype = character(),
    eining = character(),
    unik = character(),
    obligatorisk = character(),
    verdi = character(),
    verditekst = character(),
    manglande = character(),
    desimaler = integer(),
    min = numeric(),
    maks = numeric(),
    min_rimeleg = numeric(),
    maks_rimeleg = numeric(),
    min_dato = as.Date(character()),
    maks_dato = as.Date(character()),
    min_rimeleg_dato = as.Date(character()),
    maks_rimeleg_dato = as.Date(character()),
    kommentar_rimeleg = character(),
    utrekningsformel = character(),
    logikk = character(),
    kommentar = character()
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
  expect_equivalent(les_kb_oqr_base("oqr_kodebok.csv"), kb_eksempel)
})

# konverter_tekst() -----------------------------------------------------
context("konverter_tekst")

test_that("funksjonen håndterer konvertering til desimaltall", {
  tekst_til_tall_punktum = c("1.2", "1.3")
  tekst_til_tall_komma = c("1,2", "1,3")
  tekst_til_tall_heltall = c("1", "2")
  tekst_til_tall_resultat = c(1.2, 1.3)
  tekst_til_tall_heltall_resultat = c(1, 2)

  expect_identical(
    konverter_tekst(tekst_til_tall_punktum,
      regex = "^[-]?\\d+[.]?\\d*$",
      parse_funksjon = readr::parse_double
    ),
    tekst_til_tall_resultat
  )

  expect_identical(
    konverter_tekst(tekst_til_tall_komma,
      regex = "^[-]?\\d+[,]?\\d*$",
      parse_funksjon = readr::parse_double,
      locale = locale(decimal_mark = ",")
    ),
    tekst_til_tall_resultat
  )

  expect_identical(
    konverter_tekst(tekst_til_tall_heltall,
      regex = "^[-]?\\d+[.]?\\d*?$",
      parse_funksjon = readr::parse_double
    ),
    tekst_til_tall_heltall_resultat
  )
})

test_that("verdier som ikke tolkes som tall blir konvertert til NA", {
  tekst_til_tall_med_na_punktum = c("1.2", "birthYear")
  tekst_til_tall_med_na_komma = c("1,2", "birthYear")
  tekst_til_tall_resultat = c(1.2, NA_real_)

  expect_identical(
    konverter_tekst(tekst_til_tall_med_na_punktum,
      regex = "^[-]?\\d+[.]?\\d*$",
      parse_funksjon = readr::parse_double
    ),
    tekst_til_tall_resultat
  )

  expect_identical(
    konverter_tekst(tekst_til_tall_med_na_komma,
      regex = "^[-]?\\d+[,]?\\d*$",
      parse_funksjon = readr::parse_double,
      locale = locale(decimal_mark = ",")
    ),
    tekst_til_tall_resultat
  )
})

test_that("funksjonen skiller mellom *ekte* tall, og tall som er del av en tekststreng", {
  tall_i_tekst = c("1.2", "birth in 1.2 Year")
  tall_i_tekst_resultat = c(1.2, NA_real_)

  expect_identical(
    konverter_tekst(tall_i_tekst,
      regex = "^[-]?\\d+[.]?\\d*$",
      parse_funksjon = readr::parse_double
    ),
    tall_i_tekst_resultat
  )
})

test_that("funksjonen håndterer konvertering til dato", {
  tekst_til_dato = c("2020-01-15", "2014-03-10")
  tekst_til_dato_resultat = readr::parse_date(tekst_til_dato, format = "%Y-%m-%d")

  expect_identical(konverter_tekst(tekst_til_dato,
    regex = "^\\d{4}-\\d{2}-\\d{2}$",
    parse_funksjon = readr::parse_date,
    format = "%Y-%m-%d"
  ), tekst_til_dato_resultat)
})

test_that("verdier som ikke tolkes som dato blir konvertert til NA", {
  tekst_til_dato_med_na = c("2020-01-15", "birthYear")
  tekst_til_dato_res = c(readr::parse_date(tekst_til_dato_med_na[1],
    format = "%Y-%m-%d"
  ), NA)

  expect_identical(
    konverter_tekst(tekst_til_dato_med_na,
      regex = "^\\d{4}-\\d{2}-\\d{2}$",
      parse_funksjon = readr::parse_date,
      format = "%Y-%m-%d"
    ),
    tekst_til_dato_res
  )
})

test_that("funksjonen skiller mellom *ekte* dato, og dato som er del av en tekststreng", {
  dato_i_tekst = c("2020-01-15", "birthdate is 2020-01-15")
  dato_i_tekst_res = c(readr::parse_date(dato_i_tekst[1]), NA)

  expect_identical(
    konverter_tekst(dato_i_tekst,
      regex = "^\\d{4}-\\d{2}-\\d{2}$",
      parse_funksjon = readr::parse_date,
      format = "%Y-%m-%d"
    ),
    dato_i_tekst_res
  )
})

# FIXME - Denne testen kan bli overflødig når JIRA-sak (https://issuetracker.helsenord.no/browse/ABN-372) er løst
test_that("datovariabler med apostrof blir riktig konvertert", {
  tekst_til_dato_med_apostrof = c("'2020-01-15'", "birthYear")
  tekst_til_dato_med_apostrof_res = c(readr::parse_date("2020-01-15",
    format = "%Y-%m-%d"
  ), NA)

  expect_identical(
    konverter_tekst(tekst_til_dato_med_apostrof,
      regex = "^[']\\d{4}-\\d{2}-\\d{2}[']$",
      parse_funksjon = readr::parse_date,
      format = "'%Y-%m-%d'"
    ),
    tekst_til_dato_med_apostrof_res
  )
})

test_that("funksjonen gir feilmelding om inndata ikke er en tekstvektor", {
  inndata_numerisk = c(1.2, 1.3)

  expect_error(konverter_tekst(inndata_numerisk,
    regex = "^[-]?\\d+[.]\\d*$",
    parse_funksjon = readr::parse_double
  ))
})

# kb_oqr_base_til_std -----------------------------------------------------
context("kb_oqr_base_til_std")

test_that("funksjonen fjerner duplikate variabler i samme tabell, men godtar duplikat i ulike tabeller", {
  kb_duplikate_variabler = add_row(kb_tom,
    tabell = c(
      "pasreg", "pasreg", "pasreg",
      "pasreg", "basereg", "basereg"
    ),
    skjemanavn = c(
      "Oppfølging 1 år", "Oppfølging 1 år",
      "Oppfølging 2 år", "Oppfølging 2 år",
      "Basisregistrering", "Basisregistrering"
    ),
    fysisk_feltnavn = "død",
    type = "Listevariabel",
    listeverdier = c("1", "2", "1", "2", "1", "2"),
    obligatorisk = "nei",
    aktiveringsspoersmaal = "nei",
    underspoersmaal = "nei",
    listetekst = c(
      "ja", "nei", "ja",
      "nei", "ja", "nei"
    )
  )

  kb_duplikat_resultat = add_row(kb_tom_std,
    skjema_id = c(
      "pasreg", "pasreg",
      "basereg", "basereg"
    ),
    skjemanavn = c(
      "Oppfølging 1 år", "Oppfølging 1 år",
      "Basisregistrering", "Basisregistrering"
    ),
    variabel_id = "død",
    variabeltype = "kategorisk",
    obligatorisk = "nei",
    unik = "nei",
    manglande = "nei",
    verdi = c("1", "2", "1", "2"),
    verditekst = c("ja", "nei", "ja", "nei")
  )

  expect_identical(kb_oqr_base_til_std(kb_duplikate_variabler), kb_duplikat_resultat)
})
# Utvid_statusvariabel() ----------------------------------------------------------
context("utvid_statusvariabel")

test_that("funksjonen godtar flere statusvariabler når de er i ulike tabeller", {
  kb_flere_status_ok = add_row(kb_tom_mellom,
    skjema_id = c("basereg", "pasient"),
    variabeltype = c("Statusvariabel", "Statusvariabel")
  )

  kb_flere_status_ok_res = add_row(kb_tom_mellom,
    skjema_id = c(rep("basereg", 3), rep("pasient", 3)),
    variabeltype = c(
      "Listevariabel", "Listevariabel", "Listevariabel",
      "Listevariabel", "Listevariabel", "Listevariabel"
    ),
    verdi = c("-1", "0", "1", "-1", "0", "1"),
    verditekst = c(
      "Opprettet", "Lagret", "Ferdigstilt",
      "Opprettet", "Lagret", "Ferdigstilt"
    )
  )

  expect_identical(utvid_statusvariabel(kb_flere_status_ok), kb_flere_status_ok_res)
})

test_that("funksjonen gir feilmelding hvis det er flere statusvariabler i samme tabell", {
  kb_flere_status_samme = add_row(kb_tom_mellom,
    skjema_id = c("basereg", "basereg", "pasient"),
    variabeltype = c("Statusvariabel", "Statusvariabel", "Statusvariabel")
  )

  expect_error(utvid_statusvariabel(kb_flere_status_samme))
})

# valider_oqr_kb ----------------------------------------------------------

context("oqr_til_std_variabeltyper")

test_that("funksjonen returnerer riktige navn for variabeltype etter konvertering", {
  kb_ok_navn = add_row(kb_tom_mellom,
    variabeltype = c(
      "Listevariabel", "Tekstvariabel", "Stor tekstvariabel",
      "Avkrysningsboks", "Datovariabel", "Skjult variabel",
      "Tallvariabel", "Tidsvariabel", "TIMESTAMP"
    )
  )

  kb_ok_resultat = add_row(kb_tom_mellom,
    variabeltype = c(
      "kategorisk", "tekst", "tekst", "boolsk",
      "dato", "tekst", "numerisk", "kl", "dato_kl"
    )
  )

  expect_identical(oqr_til_std_variabeltyper(kb_ok_navn), kb_ok_resultat)
})

test_that("funksjonen gir feilmelding ved ukjente variabeltyper", {
  kb_ny_vartype = add_row(kb_tom_mellom,
    variabeltype = c("Listevariabel", "Tekstvariabel", "Tellevariabel")
  )

  expect_error(
    oqr_til_std_variabeltyper(kb_ny_vartype),
    "Kodeboka har variabeltypar me ikkje støttar / har standardnamn på:\nTellevariabel"
  )
})

context("sjekk_obligatorisk")
test_that("funksjonen gir forventet verdi for obligatorisk", {
  kb_obligatorisk = add_row(kb_tom_mellom,
    obligatorisk = c("ja", "ja", "nei", "nei"),
    aktiveringsspoersmaal = c("ja", "nei", "ja", "nei"),
    underspoersmaal = "nei"
  )

  kb_oblig_ok_ja = filter(kb_obligatorisk,
    obligatorisk == "ja", aktiveringsspoersmaal == "ja"
  )

  kb_oblig_ok_ja_res = add_row(kb_tom_mellom,
    obligatorisk = "ja",
    aktiveringsspoersmaal = "ja",
    underspoersmaal = "nei"
  )

  kb_oblig_ok_nei = filter(kb_obligatorisk,
    obligatorisk == "ja", aktiveringsspoersmaal == "nei"
  )

  kb_oblig_ok_nei_res = add_row(kb_tom_mellom,
    obligatorisk = "nei",
    aktiveringsspoersmaal = "nei",
    underspoersmaal = "nei"
  )

  kb_oblig_ok_nei_2 = filter(kb_obligatorisk,
    obligatorisk == "nei",
    aktiveringsspoersmaal == "ja"
  )

  kb_oblig_ok_nei_res_2 = add_row(kb_tom_mellom,
    obligatorisk = "nei",
    aktiveringsspoersmaal = "ja",
    underspoersmaal = "nei"
  )

  expect_identical(sjekk_obligatorisk(kb_oblig_ok_ja), kb_oblig_ok_ja_res)
  expect_identical(sjekk_obligatorisk(kb_oblig_ok_nei), kb_oblig_ok_nei_res)
  expect_identical(sjekk_obligatorisk(kb_oblig_ok_nei_2), kb_oblig_ok_nei_res_2)
})

test_that("funksjonen gir feilmelding hvis obligatorisk, aktiveringsspoersmaal eller underspoersmaal er NA", {
  kb_oblig_NA = add_row(kb_tom_mellom,
    variabeltype = "Listevariabel",
    aktiveringsspoersmaal = "nei",
    underspoersmaal = "nei"
  )
  kb_aktiv_NA = add_row(kb_tom_mellom,
    variabeltype = "Listevariabel",
    obligatorisk = "ja",
    underspoersmaal = "nei"
  )
  kb_under_NA = add_row(kb_tom_mellom,
    variabeltype = "Listevariabel",
    obligatorisk = "ja",
    aktiveringsspoersmaal = "nei"
  )

  expect_error(valider_oqr_kb(kb_oblig_NA))
  expect_error(valider_oqr_kb(kb_aktiv_NA))
  expect_error(valider_oqr_kb(kb_under_NA))
})

context("velg_standardkolonner")
test_that("funksjonen fungerer som forventet med riktig input og ekstra kolonner", {
  kb_ekstra = kb_tom_mellom |>
    add_column(
      ekstra = character(),
      ekstra2 = numeric(),
      ekstra3 = logical()
    ) |>
    select(ekstra, variabel_id, desimaler, ekstra2, everything())
  kb_ekstra_resultat = kb_tom_std

  expect_identical(velg_standardkolonner(kb_ekstra), kb_ekstra_resultat)
})

test_that("funksjonen gir feilmelding hvis kolonne ikke finnes i inndata", {
  kb_manglende = select(kb_tom_mellom, -variabel_id)

  expect_error(velg_standardkolonner(kb_manglende))
})

context("tildel_unike_skjemanavn_fra_skjema_id")

test_that("funksjonen gir forventede skjemanavn", {
  kb_skjemanavn = add_row(kb_tom_std,
    skjema_id = c(
      "pasreg", "basereg", "basereg",
      "pasreg", "op", "op", "ev", "basereg"
    ),
    skjemanavn = c(
      "Pasient", "Basis", "Basis", "Opskjema",
      "Pasient", "Opskjema", "Opskjema", "Basis"
    )
  )

  kb_skjemanavn_res = add_row(kb_tom_std,
    skjema_id = c(
      "pasreg", "basereg", "basereg",
      "pasreg", "op", "op", "ev", "basereg"
    ),
    skjemanavn = c(
      "Pasient", "Basis", "Basis", "Pasient",
      "Opskjema", "Opskjema", "ev", "Basis"
    )
  )

  expect_identical(tildel_unike_skjemanavn_fra_skjema_id(kb_skjemanavn), kb_skjemanavn_res)
})

test_that("funksjonen gir feilmelding hvis skjemanavn og skjema_id er overlappende uten 1-1 samsvar mellom de to", {
  kb_skjema = add_row(kb_tom_std,
    skjema_id = c("a", "b", "c"),
    skjemanavn = c("a", "c", "c")
  )

  expect_error(
    tildel_unike_skjemanavn_fra_skjema_id(kb_skjema),
    "Det finnes overlappende skjemanavn og skjema_id, og det er ikke 1-1 forhold mellom navnene"
  )
})

# legg_til_variabler_kb ---------------------------------------------------
context("legg_til_variabler_kb")

kb_legg_til_base = add_row(kb_tom_std,
  skjema_id = c(rep("basereg", 3), "pasreg"),
  skjemanavn = c(rep("basisregistrering", 3), "pasientskjema"),
  variabel_id = c("a", "b", "c", "pasientId"),
  variabeltype = c(rep("tekst", 3), "numerisk"),
  variabeletikett = c(rep("normal", 3), "id"),
  unik = c(rep("nei", 3), "ja"),
  obligatorisk = "ja",
  desimaler = c(rep(NA, 3), 0L)
)

test_that("funksjonen legger til ekstra variabler som forventet", {
  kb_legg_til_res = kb_legg_til_base |>
    add_row(
      skjema_id = c("basereg", "pasreg", "basereg"),
      skjemanavn = c("basisregistrering", "pasientskjema", "basisregistrering"),
      variabel_id = c("d", "navn", "hoyde"),
      variabeltype = c("tekst", "tekst", "numerisk"),
      variabeletikett = c("normal", "fornavn", "cm"),
      unik = c("nei", "nei", "nei"),
      obligatorisk = c("ja", "nei", "nei"),
      desimaler = c(NA, NA, 0L)
    ) |>
    arrange(fct_inorder(skjema_id))

  ekstra_data = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id, ~variabeltype, ~variabeletikett, ~unik, ~obligatorisk, ~desimaler,
    "basereg", "basisregistrering", "d", "tekst", "normal", "nei", "ja", NA,
    "basereg", "basisregistrering", "hoyde", "numerisk", "cm", "nei", "nei", 0L,
    "pasreg", "pasientskjema", "navn", "tekst", "fornavn", "nei", "nei", NA
  )

  expect_identical(
    legg_til_variabler_kb(kb_legg_til_base, ekstra_data = ekstra_data),
    kb_legg_til_res
  )
})

test_that("funksjonen gir feilmelding hvis variabel eksisterer fra før", {
  duplikat_variabel = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id, ~variabeltype, ~variabeletikett, ~unik, ~obligatorisk, ~desimaler,
    "basereg", "basisregistrering", "a", "tekst", "normal", "nei", "ja", NA
  )

  expect_error(legg_til_variabler_kb(kb_legg_til_base,
    skjema = "basereg",
    variabler = duplikat_variabel
  ),
  error = "Variabelen:\n 'a' finnes i skjema fra før"
  )
})
test_that("funksjonen gir feilmelding hvis ikke alle nødvendige verdier er inkludert", {
  ingen_variabeltype = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id, ~variabeletikett, ~unik, ~obligatorisk, ~desimaler,
    "basereg", "basisregistrering", "a", "normal", "nei", "ja", NA
  )

  expect_error(legg_til_variabler_kb(kb_legg_til_base,
    skjema = "basereg",
    variabler = ingen_variabeltype
  ),
  error = "Det mangler kolonner for nye variabler:\n variabeltype"
  )
})

test_that("det går an å legge inn ekstra kolonner som ikke er obligatorisk,
          men som er i inndata", {
  ekstra_data_ok = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id,
    ~variabeltype, ~variabeletikett, ~unik,
    ~obligatorisk, ~desimaler, ~maks_rimeleg,
    ~maks, ~verdi,
    "basereg", "basisregistrering", "hoyde", "numerisk", "cm", "nei", "ja", 0, 200, 267, "verdi"
  )

  ekstra_data_ok_res = kb_legg_til_base |>
    add_row(
      skjema_id = "basereg", skjemanavn = "basisregistrering",
      variabel_id = "hoyde", variabeltype = "numerisk",
      variabeletikett = "cm", unik = "nei",
      obligatorisk = "ja", desimaler = 0,
      maks_rimeleg = 200, maks = 267, verdi = "verdi"
    ) |>
    arrange(fct_inorder(skjema_id))

  ekstra_data_ikke_ok = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id,
    ~variabeltype, ~variabeletikett, ~unik,
    ~obligatorisk, ~desimaler, ~ikke_lov, ~basket,
    "basereg", "basisregistrering", "hoyde", "numerisk", "cm", "nei", "ja", 0, "ulovlig variabel", "ball"
  )
  feilmelding = "Det er kolonner i ekstra_data som ikke eksisterer i kodebok fra før:\nikke_lov, basket"

  expect_identical(legg_til_variabler_kb(kb_std = kb_legg_til_base, ekstra_data = ekstra_data_ok), ekstra_data_ok_res)
  expect_error(legg_til_variabler_kb(kb_std = kb_legg_til_base, ekstra_data = ekstra_data_ikke_ok), feilmelding)
})

test_that("funksjonen gir feilmelding om du prøver å legge til en variabel som allerede eksisterer", {
  duplikat = tribble(
    ~skjema_id, ~skjemanavn, ~variabel_id, ~variabeltype, ~variabeletikett, ~unik, ~obligatorisk, ~desimaler,
    "basereg", "basisregistrering", "a", "tekst", "normal", "nei", "ja", NA
  )

  expect_error(
    legg_til_variabler_kb(kb_std = kb_legg_til_base, ekstra_data = duplikat),
    "Variabel i ekstra_data eksisterer i skjema fra før:\na"
  )
})


# valider_kodebok ---------------------------------------------------------
context("valider_kodebok")

# Deler valider kodebok opp i ulike grupper som vil inneholde egne funksjoner.
# Tenker 3 eller 4 nivå.
# - Valider kb_stuktur - Kanskje overflødig gitt at utdata fra les_kb_*funksjoner er på standardformat.
# - Valider kb_skjema - Tester på skjemanivå
# - Valider kb_kolonner - Tester på kolonnenivå
# - Valider kb_variabler - Tester på variabelnivå


# Valider_kb_struktur ---------------------------------------------
context("valider kb_struktur")
# KB-struktur: (Alle disse er kanskje sånne som kan forventes å være OK basert på les_kb_*-funksjonene)
# Sjekke at standardkolonner er inkludert
# Sjekke rekkefølge for standard kolonner
# Sjekke at alle kolonner har riktig format
# Håndtere standard-fyll for kolonner (hvis disse mangler, eventuelt sjekke)
# Håndtere glisne kolonner?
# Sjekke at struktur er riktig (skjema henger sammen, variabler henger sammen)


# Valider_kb_skjema -----------------------------------------------
context("valider kb_skjema")
# skjema-nivå:

# Sjekke at skjemanavn er unikt innenfor skjemaid
test_that("funksjonen gir feilmelding hvis en skjemaid har flere skjemanavn", {
  kb_samme_navn = add_row(kb_tom_std,
    skjema_id = c("base", "base", "pasient", "pasient"),
    skjemanavn = c(
      "basisregistrering", "basisregistrering", "pasient", "pasientregistrering"
    )
  )

  expect_error(
    valider_kb_skjema(kb_samme_navn),
    "skjema_id har ikke entydig skjemanavn\nskjema_id: pasient"
  )
})

# Tester for situasjoner hvor 'kategorier' brukes
# Alle skjema skal ha minst én kategori
test_that("funksjonen gir feilmelding hvis det finnes kategorier,
          men ikke for alle skjema", {
  kb_manglende_kategori = add_row(kb_tom_std,
    skjema_id = c("base", "pasient", "tredje"),
    kategori = c("basiskategori", "pasientkategori", NA_character_)
  )

  expect_error(
    valider_kb_skjema(kb_manglende_kategori),
    "Alle skjema må ha tilhørende kategori hvis kategorier brukes. Følgende skjema_id mangler kategori:\ntredje"
  )
})

# Kategorioversikt i første rad
test_that("funksjonen gir feilmelding hvis kategorier brukes,
          men det ikke er oppgitt kategori i første rad på alle skjema", {
  kb_manglende_kategori_rad_1 = add_row(kb_tom_std,
    skjema_id = c(
      "base", "base", "base", "pasient", "pasient"
    ),
    kategori = c(
      NA_character_, "basiskategori", "basiskategori",
      "pasientkategori", NA_character_
    )
  )

  expect_error(
    valider_kb_skjema(kb_manglende_kategori_rad_1),
    "Hvis kategorier brukes må det være oppgitt kategori i første rad for alle skjema"
  )
})


# Valider_kb_kolonner ---------------------------------------------
context("valider kb_kolonner")
# Sjekke at alle variabeltyper er kjent og akseptert
test_that("funksjonen gir feilmelding hvis det finnes variabeltyper som ikke er i standardsett", {
  kb_ny_vartype = add_row(kb_tom_std,
    variabeltype = c("tekst", "numerisk", "farge")
  )

  expect_error(
    valider_kb_kolonner(kb_ny_vartype),
    "Det finnes variabeltyper som ikke er støttet:\nfarge"
  )
})

# Sjekke obligatorisk-kolonnen
test_that("funksjonen gir feilmelding hvis obligatorisk kolonnen ikke er tekstformat", {
  kb_obligatorisk_logisk = mutate(kb_tom_std,
    obligatorisk = as.logical(obligatorisk)
  )

  expect_error(valider_kb_kolonner(kb_obligatorisk_logisk))
})

# Sjekke Ja/Nei kolonner
test_that("funksjonen gir feilmelding hvis Ja/nei kolonner inneholder noe annet enn 'ja' og 'nei'", {
  kb_ja_nei = add_row(kb_tom_std,
    variabeltype = "tekst",
    obligatorisk = c("niks", "nei", "ja"),
    unik = c("nei", "ikke", "ja"),
    manglande = c("ja", "ja", "nope")
  )
  feilmelding_ja_nei = "Kolonnene obligatorisk, unik og manglande kan bare inneholde 'ja' eller 'nei'"


  expect_error(valider_kb_kolonner(kb_ja_nei[1, ]), feilmelding_ja_nei)
  expect_error(valider_kb_kolonner(kb_ja_nei[2, ]), feilmelding_ja_nei)
  expect_error(valider_kb_kolonner(kb_ja_nei[3, ]), feilmelding_ja_nei)
})

# Sjekke desimaler-kolonnen
test_that("funksjonen gir feilmelding hvis desimalkolonnen inneholder verdier mindre enn null", {
  kb_desimal_negativ = add_row(kb_tom_std,
    variabeltype = "numerisk",
    obligatorisk = "nei",
    unik = "nei",
    manglande = "nei",
    desimaler = c(-1L, 0L, 3L)
  )

  kb_desimal_ikke_heltall = add_row(kb_tom_std,
    variabeltype = "numerisk",
    obligatorisk = "nei",
    unik = "nei",
    manglande = "nei",
    desimaler = c(2, 3, 4.0)
  )

  feilmelding_desimal = "Desimalkolonnen må være et ikke-negativt heltall"

  expect_error(
    valider_kb_kolonner(kb_desimal_negativ),
    feilmelding_desimal
  )
  expect_error(valider_kb_kolonner(kb_desimal_ikke_heltall))
})

# Sjekke eining-kolonnen
test_that("funksjonen gir feilmelding hvis eining er en tom tekststreng", {
  kb_feil_eining = add_row(kb_tom_std,
    variabeltype = "tekst",
    obligatorisk = "nei",
    unik = "nei",
    manglande = "nei",
    eining = c(NA_character_, "liter", "kilo", "")
  )

  expect_error(
    valider_kb_kolonner(kb_feil_eining),
    "Eining kan ikke være en tom tekststreng"
  )
})

# Sjekke variabelnavn
test_that("funksjonen gir feilmelding hvis variabelnavn ikke starter med en bokstav, eller inneholder annet enn tall, bokstaver og '_'", {
  kb_feil_variabel_id = add_row(kb_tom_std,
    variabel_id = c("vekt", "høyde_i_cm", "2_ukers_vekt", "SUPER!"),
    variabeltype = "tekst",
    obligatorisk = "nei",
    unik = "nei",
    manglande = "nei"
  )

  expect_error(
    valider_kb_kolonner(kb_feil_variabel_id),
    "Det finnes ugyldige variabelnavn:\n2_ukers_vekt, SUPER!"
  )
})

# Valider_kb_variabler --------------------------------------------
context("valider kb_variabler")

# Variabelnivå:
test_that("funksjonen gir feilmelding hvis en variabel har flere variabeltyper", {
  kb_flere_variabeltyper = add_row(kb_tom_std,
    skjema_id = c("base", "base", "pasient", "pasient"),
    variabel_id = c("vekt", "høyde", "vekt", "høyde"),
    variabeltype = c("tekst", "numerisk", "numerisk", "numerisk")
  )

  expect_error(
    valider_kb_variabler(kb_flere_variabeltyper),
    "Variabler må ha entydige variabeltyper:\nvekt"
  )
})
test_that("funksjonen gir feilmelding hvis en variabel har flere variabeletiketter", {
  kb_flere_variabeletiketter = add_row(kb_tom_std,
    skjema_id = c("base", "pasient"),
    variabel_id = "vekt",
    variabeltype = "numerisk",
    variabeletikett = c("vekt i kg", "vekt i gram")
  )

  expect_error(
    valider_kb_variabler(kb_flere_variabeletiketter),
    "En variabel kan ikke ha flere ulike variabeletiketter:\nvekt"
  )
})

test_that("funksjonen gir feilmelding hvis en faktor har ulike verditekster for samme verdi på tvers av skjema", {
  kb_ulike_faktornivaa = add_row(kb_tom_std,
    skjema_id = c("base", "base", "base", "pasient", "pasient", "pasient"),
    variabel_id = "komplikasjon",
    variabeltype = "kategorisk",
    verdi = c("1", "2", "3", "1", "2", "3"),
    verditekst = c("hoste", "svette", "grining", "hoste", "svette", "latterkrampe")
  )

  expect_error(
    valider_kb_variabler(kb_ulike_faktornivaa),
    "Det finnes 1 avvik for listeverdi mellom skjema:\nVariabel: komplikasjon"
  )
})

test_that("funksjonen gir feilmelding hvis en boolsk variabel har 'Obligatorisk' = Nei, og 'Unik' = Ja", {
  kb_boolsk_feil = add_row(kb_tom_std,
    variabel_id = "oblig_feil",
    variabeltype = "boolsk",
    obligatorisk = "nei",
    unik = "ja"
  )

  expect_error(
    valider_kb_variabler(kb_boolsk_feil[1, ]),
    "Boolske variabler kan ikke ha Obligatorisk = 'nei' og Unik = 'ja'\nVariabel: oblig_feil"
  )
})

test_that("funksjonen gir feilmelding hvis en kategorisk variabel har duplikate verdier", {
  kb_kategorisk_feil = add_row(kb_tom_std,
    variabel_id = "komplikasjon",
    variabeltype = "kategorisk",
    verdi = c("1", "1", "2", NA_character_),
    verditekst = c("hevelse", "hevelse", "hoste", "skjelving")
  )

  expect_error(
    valider_kb_variabler(kb_kategorisk_feil[1:3, ]),
    "Kategoriske variabler må ha unike verdier\nVariabel: komplikasjon"
  )
  expect_error(
    valider_kb_variabler(kb_kategorisk_feil[2:4, ]),
    "Kategoriske variabler kan ikke ha NA som verdi\nVariabel: komplikasjon"
  )
  expect_error(
    valider_kb_variabler(kb_kategorisk_feil[1, ]),
    "Kategoriske variabler må ha minst to svaralternativ\nVariabel: komplikasjon"
  )
})

test_that("funksjonen gir feilmelding hvis en numerisk variabel har noe annet enn NA i kolonnene:
          verdi, verditekst, min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato", {
  kb_numerisk_feil = add_row(kb_tom_std,
    variabeltype = "numerisk",
    verdi = c("1", rep(NA_character_, 5)),
    verditekst = c(NA_character_, "tekst", rep(NA_character_, 4)),
    min_dato = as.Date(
      c(rep(NA_character_, 2), "10-01-2020", rep(NA_character_, 3))
    ),
    maks_dato = as.Date(
      c(rep(NA_character_, 3), "10-01-2020", rep(NA_character_, 2))
    ),
    min_rimeleg_dato = as.Date(
      c(rep(NA_character_, 4), "10-01-2020", NA_character_)
    ),
    maks_rimeleg_dato = as.Date(c(rep(NA_character_, 5), "10-10-2020"))
  )

  feilmelding_numeriske = "Numeriske variabler kan ikke ha informasjon i kolonnene:\nverdi, verditekst, min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato"

  for (i in 1:6) {
    expect_error(
      object = valider_kb_variabler(kb_numerisk_feil[i, ]),
      regexp = feilmelding_numeriske
    )
  }
})

test_that("funksjonen gir feilmelding hvis en tekstvariabel har noe annet enn NA i kolonnene:
          verdi, verditekst, desimaler, eining, min, maks, min_rimeleg, maks_rimeleg,
          min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato, kommentar_rimeleg, utrekningsformel, logikk", {
  kb_tekst_feil = add_row(kb_tom_std,
    variabeltype = "tekst",
    verdi = c("1", rep(NA_character_, 14)),
    verditekst = c(rep(NA_character_, 1), "tekst", rep(NA_character_, 13)),
    desimaler = c(rep(NA_integer_, 2), 1, rep(NA_integer_, 12)),
    eining = c(rep(NA_character_, 3), "kilo", rep(NA_character_, 11)),
    min = c(rep(NA_real_, 4), 3.5, rep(NA_real_, 10)),
    maks = c(rep(NA_real_, 5), 4.5, rep(NA_real_, 9)),
    min_rimeleg = c(rep(NA_real_, 6), 3.7, rep(NA_real_, 8)),
    maks_rimeleg = c(rep(NA_real_, 7), 4.3, rep(NA_real_, 7)),
    min_dato = as.Date(
      c(rep(NA_character_, 8), "01-01-2020", rep(NA_character_, 6))
    ),
    maks_dato = as.Date(
      c(rep(NA_character_, 9), "01-01-2020", rep(NA_character_, 5))
    ),
    min_rimeleg_dato = as.Date(
      c(rep(NA_character_, 10), "01-01-2020", rep(NA_character_, 4))
    ),
    maks_rimeleg_dato = as.Date(
      c(rep(NA_character_, 11), "01-01-2020", rep(NA_character_, 3))
    ),
    kommentar_rimeleg = c(
      rep(NA_character_, 12), "rimelige resultat", rep(NA_character_, 2)
    ),
    utrekningsformel = c(
      rep(NA_character_, 13), "a + b", rep(NA_character_, 1)
    ),
    logikk = c(rep(NA_character_, 14), "logikk")
  )

  feilmelding_tekst = "Tekstvariabler kan ikke inneholde informasjon i
kolonnene:\nverdi, verditekst, desimaler, eining, min, maks, min_rimeleg,
maks_rimeleg, min_dato, maks_dato, min_rimeleg_dato, maks_rimeleg_dato,
kommentar_rimeleg, utrekningsformel, logikk"

  for (i in 1:15) {
    expect_error(valider_kb_variabler(kb_tekst_feil[i, ]), feilmelding_tekst)
  }
})

test_that("funksjonen gir feilmelding hvis en kategorisk variabel har noe annet enn NA i kolonnene:
          eining, desimaler, min, maks, min_rimeleg, maks_rimeleg, min_dato, maks_dato,
          min_rimeleg_dato, maks_rimeleg_dato, kommentar_rimeleg, utrekningsformel, logikk", {
  kb_feil_kategorisk = add_row(kb_tom_std,
    variabel_id = c(rep("komplikasjon", 26)),
    variabeltype = "kategorisk",
    verdi = c(rep(c("hoste", "oppkast"), 13)),
    desimaler = c(1, rep(NA_integer_, 25)),
    eining = c(rep(NA_character_, 2), "kilo", rep(NA_character_, 23)),
    min = c(rep(NA_real_, 4), 3.5, rep(NA_real_, 21)),
    maks = c(rep(NA_real_, 6), 4.5, rep(NA_real_, 19)),
    min_rimeleg = c(rep(NA_real_, 8), 3.7, rep(NA_real_, 17)),
    maks_rimeleg = c(rep(NA_real_, 10), 4.3, rep(NA_real_, 15)),
    min_dato = as.Date(
      c(rep(NA_character_, 12), "01-01-2020", rep(NA_character_, 13))
    ),
    maks_dato = as.Date(
      c(rep(NA_character_, 14), "01-01-2020", rep(NA_character_, 11))
    ),
    min_rimeleg_dato = as.Date(
      c(rep(NA_character_, 16), "01-01-2020", rep(NA_character_, 9))
    ),
    maks_rimeleg_dato = as.Date(
      c(rep(NA_character_, 18), "01-01-2020", rep(NA_character_, 7))
    ),
    kommentar_rimeleg = c(
      rep(NA_character_, 20), "rimelige resultat", rep(NA_character_, 5)
    ),
    utrekningsformel = c(
      rep(NA_character_, 22), "a + b", rep(NA_character_, 3)
    ),
    logikk = c(rep(NA_character_, 24), "logikk", NA_character_)
  )

  feilmelding_kategorisk = "Kategoriske variabler kan ikke ha informasjon i kolonnene:\n
eining, desimaler, min, maks, min_rimeleg, maks_rimeleg, min_dato, maks_dato,min_rimeleg_dato, maks_rimeleg_dato,kommentar_rimeleg, utrekningsformel, logikk"

  for (i in seq(from = 1, to = 25, by = 2)) {
    expect_error(valider_kb_variabler(kb_feil_kategorisk[i:(i + 1), ]),
      regexp = feilmelding_kategorisk
    )
  }
})

test_that("funksjonen gir feilmelding hvis en ikke-kategorisk variabel har manglende = Ja", {
  kb_ikke_kat_manglande = add_row(kb_tom_std,
    variabel_id = c("navn", "vekt", "gladsak", "gladsak"),
    variabeltype = c("tekst", "numerisk", "kategorisk", "kategorisk"),
    verdi = c(NA_character_, NA_character_, "1", "2"),
    verditekst = c(NA_character_, NA_character_, "ja", "nei"),
    manglande = c("ja", "nei", "nei", "nei")
  )

  feilmelding_ikke_kategorisk_manglande = "Ikke-kategoriske variabler kan ikke ha manglende = 'ja'\nvariabel_id: navn"

  expect_error(
    valider_kb_variabler(kb_ikke_kat_manglande[1, ]),
    regexp = feilmelding_ikke_kategorisk_manglande
  )
})

test_that("funskjonen gir feilmelding hvis relasjoner mellom minimumverdier og maksverdier er feil", {
  kb_min_maks_feil = add_row(kb_tom_std,
    variabeltype = c(rep("numerisk", 4), rep("dato", 4)),
    variabel_id = c(rep("vekt", 4), rep("dato", 4)),
    min = c(10, 10, rep(NA_real_, 6)),
    maks = c(5, rep(NA_real_, 2), 5, rep(NA_real_, 4)),
    min_rimeleg = c(NA_real_, 5, 10, rep(NA_real_, 5)),
    maks_rimeleg = c(rep(NA_real_, 2), 5, 10, rep(NA_real_, 4)),
    min_dato = as.Date(c(
      rep(NA_character_, 4), "2020-01-10", "2020-01-10", rep(NA_character_, 2)
    )),
    maks_dato = as.Date(c(
      rep(NA_character_, 4), "2020-01-01", rep(NA_character_, 2), "2020-01-01"
    )),
    min_rimeleg_dato = as.Date(
      c(rep(NA_character_, 5), "2020-01-01", "2020-01-10", NA_character_)
    ),
    maks_rimeleg_dato = as.Date(
      c(rep(NA_character_, 6), "2020-01-01", "2020-01-10")
    )
  )

  for (i in 1:4) {
    expect_error(
      valider_kb_variabler(kb_min_maks_feil[i, ]),
      "Relasjon mellom minimum og maksimum verdier er ikke ivaretatt\nvariabel_id: vekt"
    )
  }
  for (i in 5:8) {
    expect_error(
      valider_kb_variabler(kb_min_maks_feil[i, ]),
      "Relasjon mellom minimum og maksimum verdier er ikke ivaretatt\nvariabel_id: dato"
    )
  }
})

test_that("funksjonen gir feilmelding hvis kommentar_rimelig finnes,
          men ingen av min_rimelig eller maks_rimelig", {
  kb_kommentar_rimeleg_feil = add_row(kb_tom_std,
    variabel_id = c("vekt1", "vekt2", "vekt3", "dato1", "dato2", "dato3"),
    variabeltype = c("numerisk", "numerisk", "numerisk", "dato", "dato", "dato"),
    min = c(rep(10, 3), rep(NA_real_, 3)),
    maks = c(rep(100, 3), rep(NA_real_, 3)),
    min_rimeleg = c(50, rep(NA_real_, 5)),
    maks_rimeleg = c(NA_real_, 80, rep(NA_real_, 4)),
    min_dato = as.Date(c(rep(NA_character_, 3), "01-01-2020", "01-01-2020", "01-01-2020")),
    maks_dato = as.Date(c(rep(NA_character_, 3), "10-01-2020", "10-01-2020", "10-01-2020")),
    min_rimeleg_dato = as.Date(c(rep(NA_character_, 3), "01-01-2020", NA_character_, NA_character_)),
    maks_rimeleg_dato = as.Date(c(rep(NA_character_, 4), "01-01-2020", NA_character_)),
    kommentar_rimeleg = "kommentar"
  )

  expect_error(
    valider_kb_variabler(kb_kommentar_rimeleg_feil[1:3, ]),
    "Kommentar_rimeleg er fylt ut, men det finnes ingen min_rimeleg eller maks_rimeleg\nvariabel_id: vekt3"
  )
  expect_error(
    valider_kb_variabler(kb_kommentar_rimeleg_feil[4:6, ]),
    "Kommentar_rimeleg er fylt ut, men det finnes ingen min_rimeleg eller maks_rimeleg\nvariabel_id: dato3"
  )
})
