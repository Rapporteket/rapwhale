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
