#' Formater tall med enhet
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Formaterer tall med tilhørende enhet automatisk bøyd til
#' entalls-/flertallsform.
#'
#' @param x Numerisk vektor.
#' @param entall Tekst som skal brukes som enhet for `x`-elementer lik 1.
#' @param flertall Tekst som skal brukes som enhet for resterende
#' `x`-elementer.
#' @param nulltekst Tekst som skal brukes for å indikere `x`-elementer lik 0.
#' Standardverdi er "0".
#'
#' @details
#' Funksjonen formaterer en numerisk vektor,
#' `x`,
#' med tilhørende enhet bøyd til
#' enten entalls- eller flertallsform.
#' Om et `x`-element er lik 1,
#' blir `x`-elementet formatert slik at `entall`-argumentet etterfølger
#' `x`-elementet.
#' Om `x`-elementet er ulik 1,
#' blir `flertall`-argumentet brukt i stedet for `entall`-argumentet.
#'
#' Noen ganger vil vi at `x`-elementer lik 0 skal vises med ord
#' (eksempelvis teksten "ingen").
#' Bruk da `nulltekst`-argumentet.
#'
#' @return
#' Tekstvektor av samme lengde som `x`.
#' Hvert element i `x` blir formatert som "tallverdi" + " " + "enhet".
#'
#' @export
#'
#' @examples
#' # Enkeltverdi inndata.
#' entall_flertall(1, "operasjon", "operasjoner")
#'
#' # Vektorverdi inndata, med ikke-standard nulltekst.
#' entall_flertall(c(0, 1, 2), "nytt", "nye", "ingen")
entall_flertall = function(x, entall, flertall, nulltekst = "0") {
  if (!is.character(entall) ||
    !is.character(flertall) ||
    !is.character(nulltekst) ||
    length(entall) != 1 ||
    length(flertall) != 1 ||
    length(nulltekst) != 1) {
    stop(
      "Argumentene 'entall', 'flertall' og 'nulltekst' må alle være ",
      "av klasse character og av lengde 1"
    )
  }
  if (length(x) == 0) {
    stop("Ingen inndata")
  }
  if (!is.numeric(x)) {
    stop(paste("Inndata må være tall (heltall/flyttall), men er:", class(x)))
  }
  if (NA %in% x) {
    stop("Inndata inneholder minst en NA-verdi")
  }
  if (!all(x >= 0)) {
    stop("Inndata inneholder minst en negativ verdi")
  }

  tall_tekst = ifelse(x == 0, nulltekst, x)
  enhet = ifelse(x == 1, entall, flertall)
  paste(tall_tekst, enhet)
}
