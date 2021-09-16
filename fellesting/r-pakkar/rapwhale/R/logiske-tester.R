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
#' Hvis `a` og `b` har ulik lengde blir den korteste brukt på ny nødvendig
#' antall ganger, likt som for logiske operatorer. Se [base::Logic] for
#' detaljer.
#'
#' @return Logisk vektor som elementvis sier om `a = TRUE` impliserer `b = TRUE`.
#' @examples
#' d = tibble::tribble(
#'   ~pas_id, ~bosted_by, ~bosted_bydel,
#'   1, "Bergen", "Åsane",
#'   2, "Bergen", "Landås",
#'   3, "Oslo", "Tjuvholmen",
#'   4, "Bergen", "Fana",
#'   5, "Oslo", "Haugenstua"
#' )
#'
#' bosted_aasane = d$bosted_bydel == "Åsane"
#' bosted_bergen = d$bosted_by == "Bergen"
#'
#' # Sjekk om bosted Åsane impliserer bosted Bergen
#' impl(bosted_aasane, bosted_bergen)
#'
#' # Eller som infiks-operator
#' bosted_aasane %impl% bosted_bergen
#' @export
impl = function(a, b) {
  if (class(a) != "logical" | class(b) != "logical") {
    stop("a og b må være logiske vektorer")
  }

  (is.na(a) | !a) | tidyr::replace_na(b, FALSE) # eg. (!a | b), men håndterer NA
}

#' @describeIn impl [impl()] som infiks-operator
#' @export
`%impl%` = impl

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
#' Hvis `a` og `b` har ulik lengde blir den korteste brukt på ny nødvendig
#' antall ganger, likt som for logiske operatorer. Se [base::Logic] for
#' detaljer.
#'
#' @return Logisk vektor som elementvis sier om `a = TRUE` er ekvivalent
#'         med `b = TRUE`.
#' @examples
#' d = tibble::tribble(
#'   ~pas_id, ~operert, ~komplikasjoner,
#'   1, "Ja", "Nei",
#'   2, "Ja", "Nei",
#'   3, "Nei", NA,
#'   4, "Nei", NA,
#'   5, "Ja", "Ja"
#' )
#'
#' er_operert = d$operert == "Ja"
#' komplikasjoner_registrert = !is.na(d$komplikasjoner)
#'
#' # Sjekk om alle som er operert har ein verdi
#' # registrert i variabelen komplikasjoner
#' ekviv(er_operert, komplikasjoner_registrert)
#'
#' # Eller som infiks-operator
#' er_operert %ekviv% komplikasjoner_registrert
#' @export
ekviv = function(a, b) {
  if (class(a) != "logical" | class(b) != "logical") {
    stop("a og b må være logiske vektorer")
  }

  impl(a, b) & impl(b, a)
}

#' @describeIn ekviv [ekviv()] som infiks-operator
#' @export
`%ekviv%` = ekviv
