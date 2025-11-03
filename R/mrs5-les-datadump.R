#' Leser inn datadump på mrs5-struktur for et enkelt skjema
#'
#' @description
#' Funksjonen leser inn datadump for et enkelt skjema og returnerer dette som
#' en tibble.
#'
#' @param filsti Plassering av datadump på disk, inkludert filnavn og extension.
#' @param kb_skjema Kodebok for det aktuelle skjemaet på kanonisk format.
#' Bruk `mrs5_hent_kodebok()` for å lese inn kodebok.
#'
#' @returns
#' Tibble for aktuelt skjema med korrekte datatyper.
#' @export
#'
#' @examples
mrs5_les_datadump = function(filsti, kb_skjema) {
  # FIXME - Add metainfo to args (birth_date)

  vars_datadump = tibble(
    variabel_id = scan(filsti,
      fileEncoding = "UTF-8-BOM",
      what = "character",
      sep = ";",
      nlines = 1,
      quiet = TRUE
    )
  )

  vars_uten_kodebok = vars_datadump$variabel_id[!vars_datadump$variabel_id %in% kb_skjema$Kodebok$variabel_id]

  hjelpe_kb = vars_datadump |>
    left_join(kb_skjema$Kodebok, by = "variabel_id") |>
    mutate(
      variabeltype = if_else(!is.na(variabeltype), variabeltype, "tekst"),
      mangler_kb = !obligatorisk
    ) |>
    left_join(koblingsliste_vartyper |> select(Kanonisk, readr), by = c("variabeltype" = "Kanonisk"))

  read_csv2(
    file = filsti,
    skip = 1,
    col_names = vars_datadump$variabel_id,
    col_types = str_c(hjelpe_kb$readr, collapse = ""),
    locale = locale(
      decimal_mark = ",", grouping_mark = "",
      date_format = "%d.%m.%Y", time_format = "%H:%M"
    )
  ) |>
    mutate(
      across(.cols = hjelpe_kb$variabel_id[hjelpe_kb$variabeltype == "boolsk"], mrs5_konverter_til_logisk),
      across(.cols = hjelpe_kb$variabel_id[hjelpe_kb$variabeltype == "dato_kl" | hjelpe_kb$variabeltype == "dato"], mrs5_håndter_dato_kl)
    ) |>
    rename_with(~ paste0("mrs_", .x, recycle0 = TRUE), .cols = all_of(vars_uten_kodebok))
}

# Funksjon som skal sjekke om datovariabler er dato / dato_kl eller / kl:

#' Konverter tekstvektor til boolsk
#'
#' @description
#' Tar inn en tekst-vektor og returnerer samme vektor som ekte boolean.
#'
#' Foreløpig er vi kun kjent med tilfeller hvor disse verdiene er kodet som 1 og
#' 0, men tidligere har det vært andre versjoner også, så funksjonen må kanskje
#' utvides for å også dekke disse.
#'
#' Konverterer nå:
#' \itemize{
#'    \item 1, True til TRUE
#'    \item 0, False til FALSE
#'    \item -1, "" til NA
#'    }
#'
#' @param x Tekstvektor som skal konverteres til boolean.
#'
#' @returns
#' returnerer `x` hvor variabler spesifisert i kodebok som boolske er
#' konvertert til ekte boolske variabler.
#'
#' @export
#'
#' @examples
#' eksempel = c("1", "0", "1", NA)
#'
#' mrs5_konverter_til_logisk(eksempel)
mrs5_konverter_til_logisk = function(x) {
  assert_that(is.character(x) | is.logical(x),
    msg = "Inndata må være en tekst-vektor."
  )
  assert_that(all(x %in% c("-1", "0", "1", "False", "True", "TRUE", "FALSE", "", NA)),
    msg = "Forventet inndata er en av: '-1', '0', '1', 'True', 'False', 'TRUE', 'FALSE'."
  )

  x[(x == "-1") | (x == "")] = NA
  (x == "1") | (x == "True") | (x == "TRUE")
}

#' Håndtere dato/dato-kl variabler fra MRS5
#'
#' I MRS finens det ingen skille mellom hva som er en dato og hva som er
#' dato-klokkeslett. Disse variablene leses inn som tekst og konverteres her
#' til henholdsvis Date eller POSIXct avhengig av om det finnes klokkeslett i
#' variabelen.
#'
#'
#' @param x Variabel som skal konverteres til dato eller dato_klokkeslett.
#'
#' @returns
#' Variabel konvertert til Date eller POSIXct-format.
#'
#' @export
mrs5_håndter_dato_kl = function(x) {
  assert_that(is.character(x), msg = "Inndata må være en tekst-vektor.")

  if (any(str_detect(x, ":"), na.rm = TRUE)) {
    withCallingHandlers(
      parse_date_time(x, orders = "dmy HM"),
      warning = function(w) {
        msg = conditionMessage(w)
        if (identical(msg, "All formats failed to parse. No formats found.")) {
          stop("Klarte ikke å finne dato-klokkeslett med forventet format: 'YYYY.MM.DD HH:MM'.", call. = FALSE)
        }
      }
    )
  } else {
    withCallingHandlers(
      parse_date_time(x, orders = "dmy"),
      warning = function(w) {
        msg = conditionMessage(w)
        if (identical(msg, "All formats failed to parse. No formats found.")) {
          stop("Klarte ikke å finne dato med forventet format: 'YYYY.MM.DD'.", call. = FALSE)
        }
      }
    )
  }
}


# read_csv2
# Kontroller kb-argument
# Hvis liste - ok, -> Anta at det er gyldig kb ?
# Hvis tekststreng - Les inn kodebok
# Hvis NULL - anta at den ligger i samme mappe som filsti ?

# Håndtering av flere skjema?
# - Forventning om at hvert skjema er lastet ned som enkeltsående tabell?
# - Hva gjør vi hvis det er flere skjema i en datadump?
# Skal det håndteres automatisk av funksjonen?
# - Skal den alltid gi ut separate objekter for hvert skjema?


# MRS5 - Alternativer ved nedlastning

# - Standard skjemadump:
# Må ha identifiserbare skjema.

# Kan velge om enum-variabler skal leveres som tall eller visningstekst.
# Funksjonen må ta høyde for begge deler.
# Trenger kanskje en kb_fyll_baklengs-funksjon for å levere alt som tall.

# Kopiering av data fra hovedskjema til underskjema. Kan fylle ut automatisk eller ikke

# Slå sammen eller ikke slå sammen variabler med samme navn fra ulike skjema.
# Problematisk i tilfeller med ulik variabelbetydning slik som feks ICD10_1 er hos NIPAR.

# Fjerning av tomme kolonner
# Åpne for at ikke alle variabler trenger å være i datadump.

# mrs5_velg_identifikator = function(kb_skjema, d_skjema) {
# }
#' #
# mrs5_samle_filnavn = function(filsti_kb) {
#' #
#   # Forventning om at alle skjema og kodebok ligger samlet i samme mappe.
#' #
#   # Henter kodebok
#   mrs5_
#   # Henter ut skjemanavn
#   # Søker i mappe etter filer som inneholder skjemanavn
#   # returnerer tibble med skjemanavn og filsti til skjema.
#' #
#   # FIXME - Legge denne inn i standard-funksjon for å håndtere filnavn
# }
