#' Formater tall med automatisk entalls-/flertallsenhet
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Formaterer tall med tilhørende enhet automatisk bøyd til
#' entalls-/flertallsform.
#'
#' @param x Numerisk vektor med minst ett element.
#'   Kan ikke inneholde `NA`-verdier.
#' @param entall Tekst som skal brukes som enhet for `x`-elementer lik `1`
#'   eller `-1`.
#' @param flertall Tekst som skal brukes som enhet for `x`-elementer ulik `1`
#'   og `-1`.
#' @param formatering Funksjon for formatering av verdiene i `x`.
#'   Tar inn `x` og må gi ut en vektor av samme lengde,
#'   enten en tekstvektor eller en vektor som kan
#'   gjøres om til tekst via [as.character()].
#' @param ... Ytterligere argumenter sendt til `formatering()`.
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
#' Formateringsfunksjonen `formatering` kan for eksempel
#' brukes til å formatere tallene i `x` med tusenskilletegn
#' eller avrundet til heltal.
#' Standard formateringsfunksjon [erstatt_0()]
#' gjør i utgangspunktet *ingen* formatering,
#' men tar argumentet `nullverdi`,
#' som kan brukes til å erstatte tallet 0
#' med en selvvalgt verdi,
#' for eksempel teksten «ingen»
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
#' boy_enhet(1, "operasjon", "operasjoner")
#' boy_enhet(34, "operasjon", "operasjoner")
#'
#' # Formatering av vektorverdier, og med ikke-standard nullverdi
#' boy_enhet(c(0, 1, 2),
#'   entall = "ny pasient",
#'   flertall = "nye pasienter",
#'   nullverdi = "ingen"
#' )
#'
#' # Formatering til LaTeX-tekst og med ett desimaltall
#' boy_enhet(c(2.67, 1, 0, pi),
#'   entall = "million",
#'   flertall = "millioner",
#'   formatering = num,
#'   desimalar = 1
#' )
boy_enhet = function(x, entall, flertall, formatering = erstatt_0, ...) {
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
  n_x = length(x)
  n_x_formatert = length(x_formatert)
  if (n_x != n_x_formatert) {
    stop(
      paste0(
        "Utdata fra formatering() må ha samme lengde som «x» ",
        "(har hhv. lengde ", n_x_formatert, " og ", n_x, ")"
      )
    )
  }
  enhet = ifelse(x %in% c(1, -1), entall, flertall)
  paste(x_formatert, enhet)
}
