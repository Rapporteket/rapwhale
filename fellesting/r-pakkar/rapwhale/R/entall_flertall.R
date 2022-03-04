#' Formater tall med automatisk entalls-/flertallsenhet
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Formaterer tall med tilhørende enhet automatisk bøyd til
#' entalls-/flertallsform.
#'
#' @param x Numerisk vektor med minst ett element.
#' `x` kan ikke inneholde `NA`-verdier.
#' @param entall Tekst som skal brukes som enhet for `x`-elementer lik 1
#' eller -1.
#' @param flertall Tekst som skal brukes som enhet for `x`-elementer ulik 1
#' og -1.
#' @param formatering Funksjon som beskriver ytterligere formatering.
#' @param ... Ytterligere argumenter sendt til `formatering`.
#'
#' @details
#' Formaterer en tallvektor,
#' `x`,
#' med tilhørende enhet bøyd til
#' enten entalls- eller flertallsform.
#' For eksempel kan tallet 1 bli formatert som «1 pasient»,
#' mens tallet 34 blir formatert som «34 pasienter».
#'
#' Alle `x`-elementer lik 1 eller -1 blir formatert med enheten `entall`,
#' mens resterende elementer blir formatert med enheten `flertall`.
#'
#' Noen ganger vil vi at `x`-elementer lik 0 skal vises med ord
#' (eksempelvis teksten «ingen»).
#' Bruk da `formatering`-argumentet sammen med funksjonen [erstatt_0()]
#' (se eksempel nedenfor).
#'
#' `formatering`-argumenetet er fleksibelt,
#' og kan for eksempel brukes til å innføre tusenskillet.
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
#' # Formatering av vektorverdier, og med ikke-standard nullverdi
#' entall_flertall(c(0, 1, 2),
#'   entall = "ny pasient",
#'   flertall = "nye pasienter",
#'   nullverdi = "ingen"
#' )
#'
#' # Formatering til ett desimaltall
#' entall_flertall(c(2.67, 1, 0, pi),
#'   entall = "million",
#'   flertall = "millioner",
#'   formatering = num,
#'   desimalar = 1
#' )
entall_flertall = function(x, entall, flertall, formatering = erstatt_0, ...) {
  if (!is.character(entall) ||
    !is.character(flertall) ||
    length(entall) != 1 ||
    length(flertall) != 1) {
    stop(
      "Argumentene «entall» og «flertall» må begge være av ",
      "klasse «character» og lengde 1"
    )
  }
  if (length(x) == 0) {
    stop("Inndata «x» inneholder ingen verdier")
  }
  if (!is.numeric(x)) {
    stop(paste("Inndata «x» må være tall, men er:", class(x)))
  }
  if (NA %in% x) {
    stop("Inndata «x» inneholder minst én NA-verdi")
  }

  x_formatert = formatering(x, ...)
  enhet = ifelse(x_formatert %in% c(1, -1), entall, flertall)
  paste(x_formatert, enhet)
}
