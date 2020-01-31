#' Leser inn variabelnavn fra datadump.
#'
#' Henter ut variabelnavn fra en datadump. Bruker filplassering som argument.
#'
#' Returnerer en vektor med variabelnavn slik de er i datadump.
#'
#' @param dd_sti Filplassering for datadump som skal leses inn. Henter kun ut variabelnavn fra datadump.

les_varnavn = function(dd_sti) {
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

# Sammenligne variabelnavn og rekkefølge mellom dd og i spesifikasjon-tibble (Bør dette flyttes til en valideringsfunksjon?)
sjekk_varnavn = function(dd_sti, varnavn_kilde) {
  varnavn_i_dd = les_varnavn(dd_sti)

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
