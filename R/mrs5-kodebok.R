# Fixme's -----------------------------------------------------------------

# FIXME - Konverter til kanonisk funksjonalitet
# FIXME - Legg inn kontroll av skjemanavn-argument i mrs5_trekk_ut_skjemanavn
# FIXME - Legg til test for at skjemanavn er korrekt skrevet i mrs5_trekk_ut_skjemanavn

# Hent mrs5-kodebok -------------------------------------------------------


# Parse rådata -------------------------------------------------------------

#' Les inn kodebok
#'
#' @description
#' Leser inn rådata fra aktuelle `skjemanavn` fra MRS5-kodebok funnet på `filsti`.
#' Kaller nødvendige hjelpefunksjoner for å lese inn de ulike fanene og objektene.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn slik de er
#' navngitt i kodebok. Hvis `skjemanavn = NULL` hentes kodebok for alle skjema inn.

#' @return
#' Listeobjekt som inneholder listene:
#' \itemize{
#'    \item versjonslogg
#'    \item metainfo
#'    \item felter
#'    \item regler
#'    }
#'
#'  Hvert skjema som leses inn får en tibble i hver liste.
#'
#' @export
#'
#' @examples
#' # Filsti til eksempeldata
#' filsti_eksempel = system.file("extdata", "eksempelkodebok.xlsx", package = "rapwhale")
#'
#' # Hente rådata for alle skjema i kodebok
#' kb_register = mrs5_parse_kodebok(filsti = filsti_eksempel, skjemanavn = NULL)
#'
#' # Hente rådata for et enkelt skjema
#' kb_testskjema = mrs5_parse_kodebok(filsti = filsti_eksempel, skjemanavn = "testskjema")
mrs5_parse_kodebok = function(filsti, skjemanavn = NULL) {
  kodebok_raa = list(
    "versjonslogg" = list(),
    "metainfo" = list(),
    "felter" = list(),
    "regler" = list()
  )

  skjemakobling = mrs5_trekk_ut_skjemanavn(filsti, skjemanavn)

  for (skjema in skjemakobling$fanenavn) {
    d_parsed_kodebok = mrs5_parse_kodebok_skjema(filsti = filsti, skjemanavn = skjema)

    # hent skjemanavn for det aktuelle fanenavnet for å navngi tibbles i listene
    skjemanavn_cased = skjemakobling$skjemanavn[skjemakobling$fanenavn == skjema]

    kodebok_raa[["versjonslogg"]][[skjemanavn_cased]] = d_parsed_kodebok[["versjonslogg"]]
    kodebok_raa[["metainfo"]][[skjemanavn_cased]] = d_parsed_kodebok[["metainfo"]]
    kodebok_raa[["felter"]][[skjemanavn_cased]] = d_parsed_kodebok[["felter"]]
    kodebok_raa[["regler"]][[skjemanavn_cased]] = d_parsed_kodebok[["regler"]]
  }

  return(kodebok_raa)
}

#' Les inn skjema fra kodebok
#'
#' Leser inn og lagrer alle aktuelle faner og objekter for ett `skjemanavn`
#' i MRS5-kodebok funnet på `filsti`.
#' Denne funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Listeobjekt med tibbles som inneholder rådataversjon av metainfo,
#' versjonslogg, felter og regler for `skjemanavn`.
#'
#' @keywords internal
mrs5_parse_kodebok_skjema = function(filsti, skjemanavn) {
  d_skjema = mrs5_parse_kodebok_meta(filsti, skjemanavn)
  d_versjonslogg = mrs5_hent_versjonslogg(d_skjema)
  d_metainfo = mrs5_hent_metainfo(d_skjema)

  d_felter = mrs5_parse_kodebok_felter(filsti, skjemanavn)

  d_regler = mrs5_parse_kodebok_regler(filsti, skjemanavn)

  return(
    list(
      "versjonslogg" = d_versjonslogg,
      "metainfo" = d_metainfo,
      "felter" = d_felter,
      "regler" = d_regler
    )
  )
}


#' les inn hovedfane for skjema
#'
#' @description
#' Leser inn hovedfane for `skjemanavn` fra MRS5-kodebok. Dette skjemaet
#' inneholder metadata for det aktuelle skjemaet.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av generelt-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_meta = function(filsti, skjemanavn) {
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = skjemanavn,
    col_names = FALSE,
    col_types = "text"
  ))

  return(d_skjemanavn)
}

#' les inn felter for skjema
#'
#' @description
#' Leser inn rådataversjon av 'felter-fane' for `skjemanavn` fra MRS5-kodebok.
#' Denne fanen inneholder informasjon om variablene i det aktuelle `skjemanavn`.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av felter-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_felter = function(filsti, skjemanavn) {
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = paste0(skjemanavn, "-felter"),
    col_names = TRUE,
    col_types = "text"
  )) |>
    add_column("Skjemanavn" = str_remove(skjemanavn, "\\d\\-"), .before = 1)

  return(d_skjemanavn)
}

#' Les inn regler for skjema
#'
#' @description
#' Leser in rådataversjon av 'regler-fane' for `skjemanavn` fra MRS5-kodebok.
#' Denne fanen inneholder infomasjon om validering som gjøres for de ulike
#' variablene ved registrering til registeret.
#' Funksjonen er ikke ment å kalles direkte.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med rådataversjon av regler-fane for `skjemanavn`
#' fra kodebok.
#'
#' @keywords internal
mrs5_parse_kodebok_regler = function(filsti, skjemanavn) {
  d_skjemanavn = suppressMessages(read_xlsx(filsti,
    sheet = paste0(skjemanavn, "-regler"),
    col_names = TRUE,
    col_types = "text"
  )) |>
    add_column("Skjemanavn" = str_remove(skjemanavn, "\\d\\-"), .before = 1)

  return(d_skjemanavn)
}

# Hjelpefunksjoner for Parse ----------------------------------------------

#' Lag skjemakobling
#'
#' @description
#' Sjekker om `skjemanavn` er en gyldig tekst-vektor hvis oppgitt. Hvis det ikke
#' er gitt en tekstvektor som argument hentes alle skjemanavn fra kodebok
#' funnet på `filsti`.
#'
#' @param filsti Tekststreng som angir filsti til kodebok.
#' @param skjemanavn Tekststreng med skjemanavn som skal leses inn.
#'
#' @return
#' Returnerer en tibble med fanenavn fra kodebok og menneskevennlige skjemanavn
#' for de samme fanene.
#'
#' @keywords internal
mrs5_trekk_ut_skjemanavn = function(filsti, skjemanavn) {
  mrs5_kontroller_argumenter(filsti, skjemanavn)

  skjemakobling = mrs5_les_skjemanavn(filsti)

  if (is.null(skjemanavn)) {
    skjema_ut = skjemakobling
  } else {
    (
      skjema_ut = skjemakobling[which(
        skjemakobling$skjemanavn %in% str_to_lower(skjemanavn)
      ), ]
    )
  }
  return(skjema_ut)
}


#' Typekontroll for argumenter til mrs5_parse-funksjoner
#'
#' @description
#' Typekontroll for argumenter som brukes i mrs5-parsefunksjoner.
#' Ikke ment å kalles direkte, men brukes for å kontrollere argumenter
#' og gi fornuftige feilmeldinger hvis det er problemer med argumentene.
#'
#' Sjekker også at skjemanavn som oppgis faktisk finnes i kodebok
#'
#' @param filsti tekststreng som indikerer plassering av kodebok.
#' @param skjemanavn en tekst-vektor med skjemanavn som skal leses inn. Hvis
#' `NULL` leses alle skjema fra kodeboken inn.
#'
#' @return
#' Returnerer TRUE hvis argumentene passerer kontroll. Hvis ikke returneres
#' aktuell feilmelding.
#'
#' @keywords internal
mrs5_kontroller_argumenter = function(filsti, skjemanavn) {
  # kontrollerer filsti
  assert_that(is.string(filsti),
    msg = "Filsti må være en tekststreng"
  )
  assert_that(has_extension(filsti, ext = "xlsx"),
    msg = "Kodebok må være en .xlsx-fil"
  )
  assert_that(is.readable(filsti),
    msg = paste0("Finner ikke kodebok på ", filsti)
  )

  # Kontrollere skjemanavn
  assert_that((is.character(skjemanavn) | is.null(skjemanavn)),
    msg = "Skjemanavn må være NULL eller en tekst-vektor"
  )

  # kontroll at skjemanavn finnes i kodebok
  skjemanavn_kobling = mrs5_les_skjemanavn(filsti)

  skjemanavn_lowercase = str_to_lower(skjemanavn)

  assert_that(
    all(skjemanavn_lowercase %in% skjemanavn_kobling$skjemanavn),
    msg = paste0(
      "Skjemanavn: ",
      str_c("«", skjemanavn_lowercase[which(!skjemanavn_lowercase %in% skjemanavn_kobling$skjemanavn)], "»",
        collapse = ", "
      ), " finnes ikke i kodebok"
    )
  )
}

#' Les inn fanenavn for mrs5-kodebok
#'
#' @description
#' Henter inn fanenavn for en MRS5-kodebok og returnerer et koblingsskjema
#' med fanenavn og menneskevennlig navn for de unike skjema som finnes i kodebok.
#'
#' @param filsti tekststreng som indikerer plassering av kodebok.
#'
#' @return
#' Returnerer en tibble med variablene fanenavn og skjemanavn.
#' Fanenavn inneholder fanenavn slik de er i kodebok,
#' skjemanavn inneholder menneskevennlig versjon som for eksempel
#' "testskjema" for fanenavn "1-Testskjema".
#'
#' @export
#'
#' @examples
#' # Filsti til eksempelkodebok
#' filsti_eksempelkodebok = system.file("extdata", "eksempelkodebok.xlsx", package = "rapwhale")
#'
#' # henter skjemanavn fra kodebok
#' skjemanavn = mrs5_les_skjemanavn(filsti = filsti_eksempelkodebok)
mrs5_les_skjemanavn = function(filsti) {
  navn_fra_kb = tibble(fanenavn = excel_sheets(filsti)) |>
    mutate(skjemanavn = str_remove(str_to_lower(fanenavn),
      pattern = "\\d+\\-"
    )) |>
    slice(seq(2, length(skjemanavn), by = 3))

  return(navn_fra_kb)
}

#' Henter ut versjonslogg for skjema
#'
#' @description
#' Henter versjonslogg fra generelt-fane for `skjemanavn`.
#' Inndata må være rådataversjon av generelt-fane for `skjemanavn`
#' hentet ut med mrs5_parse_kodebok_meta.
#'
#' @param parsed_generelt rådataversjon av generelt-fane fra kodebok.
#'
#' @return
#' Returnerer tibble med versjonslogg for `skjemanavn`.
#'
#' @keywords internal
mrs5_hent_versjonslogg = function(parsed_generelt) {
  assert_that(is.data.frame(parsed_generelt))

  skjemanavn = parsed_generelt[[1, 2]]

  # finne NA-rad for å splitte metainfo og versjonslogg
  na_rad = parsed_generelt |>
    mutate(radnr = row_number()) |>
    filter(is.na(...1)) |>
    pull(radnr)

  # eProm-skjema har en ekstra tabell i skjemanavn-fane
  siste_rad = if_else(length(na_rad) == 1,
    nrow(parsed_generelt),
    na_rad[2]
  )

  d_versjonslogg = parsed_generelt[seq(na_rad[1] + 1, siste_rad, 1), ] |>
    row_to_names(row_number = 1) |>
    clean_names() |>
    add_column("Skjemanavn" = skjemanavn, .before = 1)

  return(d_versjonslogg)
}

#' Henter ut metainfo for skjema
#'
#' @description
#' Henter metainfo fra generelt-fane for `skjemanavn`.
#' Inndata må være rådataversjon av generelt-fane for `skjemanavn`
#' hentet ut med mrs5_parse_kodebok_meta.
#'
#' @param parsed_generelt rådataversjon av generelt-fane fra kodebok.
#'
#' @return
#' Returnerer tibble med metainfo for `skjemanavn`.
#'
#' @keywords internal
mrs5_hent_metainfo = function(parsed_generelt) {
  assert_that(is.data.frame(parsed_generelt))

  # finne NA-rad
  na_rad = parsed_generelt |>
    mutate(radnr = row_number()) |>
    filter(is.na(...1)) |>
    pull(radnr)

  na_rad = na_rad[1]

  d_metainfo = parsed_generelt |>
    select(...1, ...2) |>
    slice_head(n = na_rad - 1) |>
    rename("metavariabel" = ...1, "metaverdi" = ...2) |>
    pivot_wider(
      names_from = "metavariabel",
      values_from = "metaverdi"
    ) |>
    clean_names()

  return(d_metainfo)
}

# Konverter til kanonisk --------------------------------------------------

mrs5_konverter_til_kanonisk = function(kb_parsed_raa) {
  mrs5_lag_kanonisk_kb()
}

# Hjelpefunksjoner kanonisk -----------------------------------------------

# kodebok
mrs5_lag_kanonisk_kb = function(kb_parsed_raa) {
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer
  # kb[[kodebok]] på kanonisk format.
}

###
mrs5_lag_skjema_id = function(kb_parsed_raa) {
  # Henter ut skjemanavn og genererer maskinlesbar skjema_id.
  skjema_id = janitor::make_clean_names(names(kb_parsed_raa_panda$versjonslogg),
    replace = c(
      `'` = "",
      `"` = "",
      `%` = "_percent_",
      `#` = "_number_",
      `å` = "aa",
      `æ` = "ae",
      `ø` = "oe"
    )
  )

  return(skjema_id)
}

mrs5_kanonisk_variabeltype = function() {
  # Konverter variabeltype til internt navn.

  # lage datasett med kolonne for alle kilder + kanonisk og legge i rapwhale?
}

mrs5_definer_obligatorisk = function() {
  # Definer hvilke variabler som er obligatorisk å fylle ut.
}

mrs5_identifiser_variabler_med_regler = function() {
  # Definer hvilke variabler som har regler tilknyttet.
}

# kategoriske
mrs5_lag_kanonisk_kategorisk = function() {
  # Tar inn rådataversjon fra mrs5_parse_kodebok_felter og returnerer
  # kb[[kategoriske]] på kanonisk format.
}

###
mrs5_ekspander_kategoriske = function() {
  # Fylle ut kb_kategorisk med alle verdi/verditekst-kombinasjoner.
}

mrs5_definer_manglende = function() {
  # Definere hvilke verdier/verditekster som skal regnes som manglande.
}

# regler
mrs5_lag_kanonisk_regler = function() {
  # Tar inn rådataversjon fra mrs5_parse_kodebok_regler og returnerer
  # kb[[regler]] på kanonisk format.
}

###
mrs5_definer_regeltype = function() {}

mrs5_definer_målvariabel = function() {}

mrs5_trekk_ut_regelverdi = function() {}

mrs5_trekk_ut_feilmelding = function() {}

mrs5_returner_manglende_regler = function() {
  # Gir ut en oversikt over regler som ikke kunne konverteres
}

# Validering --------------------------------------------------------------
mrs5_valider_kodebok = function(kodebok_kanonisk) {
  # Tar inn kodebok på kanonisk format og kaller på valideringsregler vha
  # hjelpefunksjoner for å validere de ulike kodebok-seksjonene.

  # Kaller på mrs5_valider_kodebok_kb, kategorisk og regler.
}

###
mrs5_valider_kodebok_kb = function(kodebok_felter) {
  # Valideringsregler for kodebok_felter.
}

mrs5_valider_kodebok_kategorisk = function(kodebok_kategorisk) {

}

mrs5_valider_kodebok_regler = function(kodebok_regler) {

}
