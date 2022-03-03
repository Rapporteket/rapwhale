#' Erstatt tall lik 0 med valgfri verdi
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Erstatter alle tall som er lik 0 med en valgfri verdi.
#'
#' @param x Numerisk vektor.
#' @param nullverdi Verdi (tall eller tekst)
#'   som skal erstatte `x`-elementer lik 0.
#'
#' @details
#' Alle verdier nøyaktig lik 0
#' blir erstattet med verdien `nullverdi`.
#' Funksjonen kan for eksempel brukes til
#' å erstatte tallet 0 med teksten «ingen» i løpende tekst.
#'
#' Funksjonen er hovedsakelig laget for å brukes
#' sammen med [entall_flertall()].
#'
#' @return
#' Vektor av samme lengde og innhold som `x`,
#' med 0-verdier byttet ut med `nullverdi`.
#' Verdiene blir om nødvendig gjort om til
#' å ha same type som `nullverdi`
#' (oftest tekst eller tall),
#' etter standard R-regler.
#'
#' @export
#'
#' @examples
#' erstatt_0(c(0, 1, 2), "ingen")
#' erstatt_0(c(0, 1, 2), -99)
#' erstatt_0(c(0, 1, 2), NA)
