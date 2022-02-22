#' Formater tall med automatisk entalls-/flertallsenhet
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Formaterer tall med tilhørende enhet automatisk bøyd til
#' entalls-/flertallsform.
#'
#' @param x Numerisk vektor.
#' @param entall Tekst som skal brukes som enhet for `x`-elementer lik 1.
#' @param flertall Tekst som skal brukes som enhet for `x`-elementer ulik 1.
#' @param nulltekst Tekst som skal erstatte `x`-elementer lik 0.
#'
#' @details
#' Formaterer en tallvektor,
#' `x`,
#' med tilhørende enhet bøyd til
#' enten entalls- eller flertallsform.
#' For eksempel kan tallet 1 bli formatert som «1 pasient»,
#' mens tallet 34 blir formatert som «34 pasienter».
#'
#' Alle `x`-elementer lik 1 blir formatert med enheten `entall`,
#' mens resterende elementer blir formatert med enheten `flertall`.
#'
#' Noen ganger vil vi at `x`-elementer lik 0 skal vises med ord
#' (eksempelvis teksten «ingen»).
#' Bruk da `nulltekst`-argumentet
#' (se eksempel nedenfor).
#'
#' @note
#' Funksjonen håndterer også desimaltall,
#' med samme regler,
#' også for tall der desimaldelen slutter på 1.
#' Det blir for eksempel «2,1 millioner»,
#' ikke «2,1 million»,
#' selv om begge skrivemåtene er tillatt på norsk.
#'
#' @return
#' Tekstvektor av samme lengde som `x`.
#' Hver tallverdi i `x` blir formatert som tallverdien + mellomrom + enhet.
#'
#' @export
#'
#' @examples
#' # Formatering av enkeltverdier
#' entall_flertall(1, "operasjon", "operasjoner")
#' entall_flertall(34, "operasjon", "operasjoner")
#'
#' # Formatering av vektorverdier, og med ikke-standard nulltekst
#' entall_flertall(c(0, 1, 2),
#'   entall = "ny pasient",
#'   flertall = "nye pasienter",
#'   nulltekst = "ingen"
#' )
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
