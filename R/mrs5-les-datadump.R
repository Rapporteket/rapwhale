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


# Hjelpefunksjoner --------------------------------------------------------

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

mrs5_velg_identifikator = function() {
  
  # Ta inn fnr eller pasientguid og returnere anonymisert PID. 
  # Bruke PID i rapporter og analyse for å gjøre rapporter mulig å kjøre 
  # uavhengig av hvilken datadump man bruker. 
  # 
}

mrs5_samle_filnavn = function(filsti_kb) {
  # Forventninger:
  # Alle filer som skal leses inn ligger i samme mappe
  # - Kodebok + alle aktuelle datadumper
  # Alle datadumper kan gjenkjennes basert på filnavn og fanenavn i kodebok
  # Alle datadumper inneholder kun data fra det aktuelle skjema.

  # Denne funksjonen vil da trekke ut alle aktuelle filer basert på navn
  # Oppgir filsti for kodebok (fullstendig)
  # Trekker ut mappe derfra og kobler filnavn mot skjema.

  # Bruker denne funksjonen i feks. les_data_nipar() som da leser inn alle
  # aktuelle skjema.
  # FIXME - Må flyttes og bytte navn til noe generelt da denne kan være 
  # grei for OQR-registre også 
}
