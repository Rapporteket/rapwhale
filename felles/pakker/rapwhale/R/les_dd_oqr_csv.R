#' Les inn variabelnavn fra datadump
#'
#' Henter ut variabelnavn fra en datadump. Bruker filplassering som argument.
#'
#' Returnerer en vektor med variabelnavn slik de er i datadump.
#'
#' @param dd_sti Filplassering for datadump som skal leses inn. Henter kun ut variabelnavn fra datadump.
les_varnavn = function(dd_sti) {
  stopifnot(is.character(dd_sti) & length(dd_sti) == 1)

  varnavn = scan(dd_sti,
    fileEncoding = "UTF-8-BOM",
    what = "character", sep = ";",
    nlines = 1, quiet = TRUE
  )

  if (anyDuplicated(varnavn) != 0) {
    stop("Variabler i datadump kan ikke ha samme navn")
  }
  varnavn
}

#' Sammenlign variabelnavn
#'
#' Funksjon som sjekker at variabelnavn i en datadump tilsvarer de variabelnavn som er oppgitt i en spesifikasjon/kodebok.
#' Tar inn fil-plassering til en data-fil eller en tibble/dataramme. Returnerer feilmelding hvis det er ulike navn i
#' datasett og spesifikasjon. Returnerer en advarsel om variabelnavn er i ulik rekkefølge i datasett og spesifikasjon.
#'
#' @param data Enten en tekst-streng som inneholder sti til en data-fil, eller en tibble/dataramme.
#' @param varnavn_kilde En vektor med navn slik det skal være i datasettet.
sammenlign_variabelnavn = function(data, varnavn_kilde) {
  if (is.character(data)) {
    varnavn_i_dd = les_varnavn(data)
  } else {
    varnavn_i_dd = colnames(data)
  }

  if (sum(!is.na(match(varnavn_i_dd, varnavn_kilde))) != length(varnavn_i_dd)) {
    stop(error = "Variabelnavn i spesifikasjon stemmer ikke overens med variabelnavn i datadump")
  }
  if (any(match(varnavn_i_dd, varnavn_kilde) != seq(1:length(varnavn_i_dd)))) {
    warning("Variabelnavn har ulik rekkefølge i datadump og spesifikasjon")
  }
}

# Håndtering av spesielle tilfeller, variabeltype = boolsk og dato_kl
oqr_boolsk_til_boolsk = function(x) {

  # Sjekk først at det berre er gyldige verdiar
  er_gyldig = (x %in% c("0", "1")) | is.na(x)
  if (!all(er_gyldig)) {
    stop(error = "Det finnes ugyldige verdier for en boolsk variabel. (Må være 0,1 eller NA)")
  } else {
    x == 1 # Gjer om til boolsk variabel
  }
}

# Funksjon som konverterer boolske variabler til "ekte" boolske.
konverter_boolske = function(d, vartype) {
  ind = which(vartype == "boolsk")
  d = d %>%
    mutate_at(ind, oqr_boolsk_til_boolsk)
  d
}

# Funksjon som konverterer dato_kl variabler fra character til DD.MM.YYYY HH:MM format.
konverter_dato_kl = function(d, vartype) { # fixme - Hvordan skal den reagere på dato_kl i feil format?
  ind = which(vartype == "dato_kl")
  d = d %>%
    mutate_at(ind, readr::parse_datetime,
      format = "%d.%m.%Y %H:%M",
      locale = locale(
        decimal_mark = ",",
        tz = "Europe/Oslo"
      )
    )
  d
}

#' Leser inn OQR-datadump i csv-format
#'
#' Funksjon for å lese inn datadump for oqr-register. Tar innn en filplassering og en spesifikasjonstibble.
#' Tibble inneholder variabelnavn fra datadump, eventuelt nye variabelnavn, variabeltype og kolonnespesifikasjon.
#'
#' Det returneres en tibble med nye variabelnavn og variabeltype som er oppgitt i spesifikasjon.
#' Hvis det er uoverenstemmelser mellom datadump og spesifikasjon vil det gis feilmeldinger.
#'
#' @param dd_sti Filplassering for datadump i csv format.
#' @param oqr_specs Tibble med spesifikasjon for hvilken struktur datadump har. Inneholder kolonnene varnavn, nye_variabelnavn, variabeltype og kolonnespesifikasjon.
les_oqr_csv_base = function(dd_sti, csv_bokstav, varnavn_kilde, nye_varnavn, vartype) {

  # hent kolonnespec på riktig format
  kol_type = str_c(csv_bokstav, collapse = "")

  names = tibble(varnavn_kilde, nye_varnavn)
  names = names %>%
    mutate(col_names = case_when(
      is.na(nye_varnavn) ~ varnavn_kilde,
      !is.na(nye_varnavn) ~ nye_varnavn
    ))

  # Definere innstillinger for locale som er likt for alle OQR-register.
  oqr_lokale = readr::locale(
    decimal_mark = ",",
    date_format = "%d.%m.%Y", time_format = "%H:%M",
    tz = "Europe/Oslo"
  )

  # les inn datasett
  d = readr::read_delim(dd_sti,
    delim = ";",
    locale = oqr_lokale,
    skip = 1,
    col_names = names$col_names,
    col_types = kol_type
  )

  readr::stop_for_problems(d)
  d
}

#' Les inn csv-fil
#'
#' Funksjon som leser inn en csv-fil fra en filplassering med angitt ariabeltype fra en spesifikasjons-tibble.
#' Faktorer, tekstvariabler, dato med klokkeslett og
#'
#' @param dd_sti Filplassering for datadump som skal leses inn
#' @param spesifikasjon Tibble med fire kolonner. Inneholder varnavn_kilde, nye_variabelnavn, vartype og kolonnetype.
les_csv_base = function(dd_sti, spesifikasjon) {
  kol_type = str_c(spesifikasjon$kolonnetype, collapse = "")

  d = readr::read_delim(dd_sti,
    delim = ";",
    locale = readr::locale(
      decimal_mark = ",", date_format = "%d.%m.%Y",
      time_format = "%H:%M", tz = "Europe/Oslo"
    ),
    col_types = kol_type
  )
  readr::stop_for_problems(d)
  d
}

# Hjelpefunksjon for å lese inn en datadump fra csv-fil for et OQR-register.
les_oqr_csv_v2 = function(dd_sti, csv_bokstav, varnavn_kilde, nye_varnavn, vartype) {
  varnavn = les_varnavn(dd_sti)
  sammenlign_variabelnavn(dd_sti, varnavn_kilde)

  d = les_oqr_csv_base(dd_sti, csv_bokstav, varnavn_kilde, nye_varnavn, vartype)

  d = konverter_boolske(d, vartype)
  d = konverter_dato_kl(d, vartype)
  # d = konverter_kategorisk(d, vartype)

  d
}
