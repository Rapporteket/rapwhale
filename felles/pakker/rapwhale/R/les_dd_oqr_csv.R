#' Les inn variabelnavn fra datadump
#'
#' Henter ut variabelnavn fra en datadump. Bruker filplassering som argument.
#'
#' Returnerer en vektor med variabelnavn slik de er i datadump.
#'
#' @param adresse Filplassering for datadump som skal leses inn. Henter kun ut variabelnavn fra datadump.
les_varnavn = function(adresse, formatspek) {
  stopifnot(is.character(adresse) & length(adresse) == 1)

  varnavn = scan(adresse,
    fileEncoding = formatspek$filkoding,
    what = "character", sep = formatspek$skilletegn,
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

#' Les inn csv-fil
#'
#' Funksjon som leser inn en csv-fil fra en filplassering med angitt variabeltype fra en spesifikasjons-tibble.
#' Aksepterte variabeltyper er: tekst, desimaltall, heltall, boolsk, dato, dato_kl og klokkeslett.
#' Variabelnavn kan endres ved å inkludere en vektor med nye variabelnavn.
#'
#' @param adresse Filplassering for datadump som skal leses inn.
#' @param spesifikasjon Tibble med tre kolonner. Inneholder varnavn_kilde, varnavn_resultat og vartype.
#' @param formatspek Liste som inneholder informasjon om hvordan csv-fil er spesifisert. Gjelder desimaltegn, datoformat, klokkeslettformat, tidssone, boolsk_sann, boolsk_usann
les_csv_base = function(adresse, spesifikasjon, formatspek) {

  # Lager en col_types streng basert på innmat. Sorterer variabelrekkefølge etter datadump
  kolnavn_fil = les_varnavn(adresse, formatspek)
  kolnavn_spek = spesifikasjon$varnavn_kilde
  testthat::expect_identical(sort(kolnavn_fil), sort(kolnavn_spek))
  radnr = match(kolnavn_fil, kolnavn_spek)
  stopifnot(all(!is.na(radnr)))
  spesifikasjon = spesifikasjon[radnr, ]

  d = readr::read_delim(adresse,
    delim = formatspek$skilletegn,
    locale = readr::locale(
      decimal_mark = formatspek$desimaltegn,
      date_format = formatspek$dato,
      time_format = formatspek$klokkeslett,
      tz = formatspek$tidssone
    ),
    col_types = str_c(spesifikasjon$kolonnetype, collapse = ""),
    col_names = spesifikasjon$varnavn_resulatat
  )
  readr::stop_for_problems(d)
  d

  # Konverter tidsvariabler
  varnavn_boolske = spesifikasjon$varnavn_resultat[spesifikasjon$variabeltype == "boolsk"]
  d = mutate_at(d, varnavn_boolske,
    readr::parse_datetime,
    orders = formatspek$dato_kl, locale = locale(formatspek$tidssone)
  )

  d = mutate_at(d, varnavn_boolske,
    konverter_boolske,
    boolsk_usann = formatspek$boolsk_usann,
    boolsk_sann = formatspek$boolsk_sann
  )

  # Konverter_boolsk:
  sjekk(is_empty(intersect(boolsk_usann, boolsk_sann)))
  x[x %in% boolsk_usann] = FALSE
  x[x %in% boolsk_sann] = TRUE

  # Konvertering og fiksing av diverse variabeltyper
}

# OQR-spesifikk funksjon for å lese inn csv vha les_csv_base.
les_csv_oqr = function(adresse, spesifikasjon, formatspek) {
}
