# Generelle funksjoner for å teste logikker

#' Sjekk om sann `a` impliserer sann `b`
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Tar inn to logiske vektorer `a` og `b`, og gir ut en logisk vektor som
#' elementvis sier om `a = TRUE` impliserer `b = TRUE`.
#'
#' @param a Logisk vektor.
#' @param b Logisk vektor som elementvis skal sjekkes mot `a`.
#'
#' @details
#' Funksjonen tar inn to logiske vektorer `a` og `b`, sjekker elementvis om
#' `a = TRUE` impliserer `b = TRUE`.
#'
#' Den gir ut en logisk vektor som er `TRUE` for hvert elementpar fra
#' `a` og `b` der `a = FALSE`, `a = NA`, eller både
#' `a` og `b = TRUE`, og `FALSE` ellers. Den gir altså ut `FALSE` for
#' elementpar som motstrider at `a = TRUE` impliserer `b = TRUE`.
#'
#' Funksjonen tilsvarer altså `!a | b`, men håndterer `NA` som `FALSE`.
#'
#' @return Logisk vektor som elementvis sier om `a = TRUE` impliserer `b = TRUE`.
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

#' Sjekk om sann `a` er ekvivalent med sann `b`
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Tar inn to logiske vektorer `a` og `b`, og gir ut en logisk vektor som
#' elementvis sier om `a = TRUE` er ekvivalent med `b = TRUE`.
#'
#' @param a Logisk vektor som elementvis skal sjekkes mot `b`.
#' @param b Logisk vektor som elementvis skal sjekkes mot `a`.
#'
#' @details
#' Funksjonen tar inn to logiske vektorer `a` og `b`, sjekker elementvis om
#' `a = TRUE` er ekvivalent med `b = TRUE`.
#'
#' Den gir ut en logisk vektor som er `TRUE` for hvert elementpar fra
#' `a` og `b` der både `a` og `b = TRUE`, eller både `a` og `b = FALSE`
#' eller `NA`, og `FALSE` ellers. Den gir altså ut `FALSE` for
#' elementpar som motstrider at `a = TRUE` er ekvivalent med `b = TRUE`.
#'
#' Funksjonen tilsvarer altså `a == b`, men håndterer `NA` som `FALSE`.
#'
#' @return Logisk vektor som elementvis sier om `a = TRUE` er ekvivalent
#'         med `b = TRUE`.
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
