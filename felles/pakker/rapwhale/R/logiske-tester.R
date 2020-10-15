# Generelle funksjoner for å teste logikker

#' Sjekk om 'a' sann impliserer 'b' sann
#'
#' @description
#' Sann hvis og bare hvis 'a' sann impliserer 'b' sann
#' (og håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
#' aldri NA)
#'
#' @param a Variabel som hvis sann impliserer at variabel 'b' er sann.
#' @param b Variabel som er sann hvis 'a' er sann.
#'
#' @return TRUE eller FALSE
#' @export
impl = function(a, b) {
  (is.na(a) | !a) | tidyr::replace_na(b, FALSE) # eg. (!a | b), men håndterer NA
}

#' Sjekk om 'a' sann impliserer 'b' sann, og omvendt
#'
#' @description
#' Sann hvis og bare hvis 'a' sann impliserer 'b' sann, og omvendt
#' (og håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
#' aldri NA)
#'
#' @param a Variabel som hvis sann impliserer at variabel 'b' er sann,
#'     og som er sann hvis variabel 'b' er sann.
#' @param b Variabel som er sann hvis 'a' er sann, og som hvis sann
#'     impliserer at variabel 'a' er sann.
#'
#' @return TRUE eller FALSE
#' @export
dobbelimpl = function(a, b) {
  impl(a, b) & impl(b, a)
}
