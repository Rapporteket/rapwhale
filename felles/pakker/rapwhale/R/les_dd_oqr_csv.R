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
