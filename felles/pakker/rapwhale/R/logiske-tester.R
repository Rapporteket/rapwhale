# Generelle funksjoner for å teste logikker

#' Sjekk om 'a' sann impliserer 'b' sann
#'
#' @description
#' Sann hvis og bare hvis 'a' sann impliserer 'b' sann
#' (håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
#' aldri NA)
#'
#' @param a Variabel som hvis sann impliserer at variabel 'b' er sann.
#' @param b Variabel som er sann hvis 'a' er sann.
#'
#' @return TRUE eller FALSE
#' @examples
#' d_gyldig_eks = tibble::tribble(
#'   ~pas_id, ~bosted_by, ~bosted_bydel,
#'   1, "Bergen", "Åsane",
#'   2, "Bergen", "Landås",
#'   3, "Oslo", "Tjuvholmen",
#'   4, "Bergen", "Fana",
#'   5, "Oslo", "Haugenstua"
#' )
#' impl(d_gyldig_eks$bosted_bydel == "Åsane", d_gyldig_eks$bosted_by == "Bergen")
#' @export
impl = function(a, b) {
  (is.na(a) | !a) | tidyr::replace_na(b, FALSE) # eg. (!a | b), men håndterer NA
}

#' Sjekk om 'a' sann er ekvivalent med 'b' sann
#'
#' @description
#' Sann hvis og bare hvis 'a' sann er ekvivalent med 'b' sann
#' (håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
#' aldri NA)
#'
#' @param a Variabel som hvis sann ekvivalerer at variabel 'b' er sann.
#' @param b Variabel som hvis sann ekvivalerer at variabel 'a' er sann.
#'
#' @return TRUE eller FALSE
#' @examples
#' d_gyldig_eks = tibble::tribble(
#'   ~pas_id, ~operert, ~komplikasjoner,
#'   1, "Ja", "Nei",
#'   2, "Ja", "Nei",
#'   3, "Nei", NA,
#'   4, "Nei", NA,
#'   5, "Ja", "Ja"
#' )
#' ekviv(d_gyldig_eks$operert == "Ja", !is.na(d_gyldig_eks$komplikasjoner))
#' @export
ekviv = function(a, b) {
  impl(a, b) & impl(b, a)
}
