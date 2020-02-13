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
  varnavn
}

# Håndtering av spesielle tilfeller, variabeltype = boolsk og dato_kl
konverter_boolske = function(x, boolsk_usann, boolsk_sann) {

  # Sjekk først at det berre er gyldige verdiar
  mulige_verdier = c(boolsk_usann, boolsk_sann, NA)
  er_gyldig = (x %in% mulige_verdier)
  er_ugyldig = x[!er_gyldig]
  overlapp = intersect(boolsk_usann, boolsk_sann)

  if (!rlang::is_empty(overlapp)) {
    stop(error = paste0("boolsk_sann og boolsk_usann kan ikke inneholde samme verdier"))
  }
  if (!all(er_gyldig)) {
    ugyldige = paste0(er_ugyldig, collapse = ",")
    mulige = paste0(mulige_verdier, collapse = ",")
    stop(error = paste0("Det finnes ugyldige verdier for en boolsk variabel: ", ugyldige, "\nMulige verdier er: ", mulige))
  }

  # Konverterer til logisk vektor
  x_boolsk = rep(NA, length(x))
  x_boolsk[x %in% boolsk_usann] = FALSE
  x_boolsk[x %in% boolsk_sann] = TRUE
  x_boolsk
}

#' Konverter variabeltype fra standard format til readrformat
#'
#' Funksjonen tar inn en vektor med variabeltyper oppgitt på vårt standardformat.
#' Returnerer en sammenslått tekststreng med gyldige coltypes for bruk i diverse readrfunksjoner.
#'
#' @param vartyper Vektor med variabeltyper oppgitt på standardformat.
#' Mulige typer er: tekst, desimalltall, heltall, boolsk, dato, dato_kl og kl.
std_koltype_til_readr_koltype = function(vartype) {
  if (length(vartype) == 0) {
    return("")
  }
  if (any(is.na(vartype))) {
    stop("Variabeltype må defineres for alle variabler")
  }

  koblingstabell = tibble::tribble(
    ~vartype, ~koltype,
    "tekst", "c",
    "desimaltall", "d",
    "heltall", "i",
    "boolsk", "c",
    "dato", "D",
    "dato_kl", "c",
    "kl", "t"
  )

  ind = match(vartype, koblingstabell$vartype)

  if (any(is.na(ind))) {
    vartype_mangler = paste0("'", vartype[is.na(ind)], "'", collapse = ", ")
    stop("Ukjent variabeltype: ", vartype_mangler)
  }

  koltype = str_c(koblingstabell$koltype[ind], collapse = "")
  koltype
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

  # Lager en kolonnenavn streng basert på innmat. Sorterer variabelrekkefølge etter datadump
  kolnavn_fil = les_varnavn(adresse, formatspek)
  kolnavn_spek = spesifikasjon$varnavn_kilde
  testthat::expect_identical(sort(kolnavn_fil), sort(kolnavn_spek))
  radnr = match(kolnavn_fil, kolnavn_spek)
  stopifnot(all(!is.na(radnr)))
  spesifikasjon = spesifikasjon[radnr, ]

  d = readr::read_delim(adresse,
    delim = formatspek$skilletegn, skip = 1,
    locale = readr::locale(
      decimal_mark = formatspek$desimaltegn,
      date_format = formatspek$dato,
      time_format = formatspek$klokkeslett,
      tz = formatspek$tidssone
    ),
    col_types = std_koltype_til_readr_koltype(spesifikasjon$vartype),
    col_names = spesifikasjon$varnavn_resultat
  )

  readr::stop_for_problems(d)

  # Konverter dato_kl
  # fixme: Fjern denne og oppdater std_koltype_til_readr_koltype() når
  #        https://github.com/tidyverse/readr/issues/642 er fiksa
  varnavn_dato_kl = spesifikasjon$varnavn_resultat[spesifikasjon$vartype == "dato_kl"]
  d = mutate_at(
    d, varnavn_dato_kl,
    ~ readr::stop_for_problems(
      readr::parse_datetime(., format = formatspek$dato_kl)
    )
  )

  # Konverter boolske
  varnavn_boolske = spesifikasjon$varnavn_resultat[spesifikasjon$vartype == "boolsk"]
  d = mutate_at(d, varnavn_boolske,
    konverter_boolske,
    boolsk_usann = formatspek$boolsk_usann,
    boolsk_sann = formatspek$boolsk_sann
  )

  d
}
