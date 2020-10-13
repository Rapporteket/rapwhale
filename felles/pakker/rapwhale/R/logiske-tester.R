# Generelle funksjoner for å teste logikker

#' Sjekk om 'a' sann impliserer 'b' sann
#'
#' @description
#' Sann hvis og bare hvis 'a' sann impliserer 'b' sann
#' (og håndterer NA-verdier fint, og gir alltid ut TRUE eller FALSE,
#' aldri NA)
#'
#' @param a
#' @param b
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
#' @param a
#' @param b
#'
#' @return TRUE eller FALSE
#' @export
dobbelimpl = function(a, b) {
  impl(a, b) & impl(b, a)
}
