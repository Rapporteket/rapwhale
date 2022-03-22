#' Erstatt tall lik 0 med valgfri verdi
#'
#' @description
#' `r lifecycle::badge("maturing")`
#'
#' Erstatter alle tall som er lik 0 med en valgfri verdi.
#'
#' @param x Numerisk vektor.
#' @param nullverdi Verdi (typisk tall eller tekst)
#'   som skal erstatte `x`-elementer lik 0.
#'
#' @details
#' Alle verdier nøyaktig lik 0
#' blir erstattet med verdien `nullverdi`.
#' Funksjonen kan for eksempel brukes til
#' å erstatte tallet 0 med teksten «ingen» i løpende tekst.
#'
#' Funksjonen er hovedsakelig laget for å brukes
#' sammen med [boy_enhet()].
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
#'
#' # Merk at utdata bare blir gjort om til tekst *om nødvendig*
#' erstatt_0(c(2, 1), "ingen") # Tall
#' erstatt_0(c(2, 0, 1), "ingen") # Tekst
erstatt_0 = function(x, nullverdi = 0) {
  if (!is.numeric(x)) {
    stop(paste("Inndata «x» må være tall, men er:", class(x)))
  }
  if (length(nullverdi) != 1) {
    stop("«nullverdi» må ha nøyaktig 1 element")
  }

  if (any(x == 0, na.rm = TRUE)) { # Unngå unødvendig typeomgjering
    x[x == 0] = nullverdi
  }
  x
}
