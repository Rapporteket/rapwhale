#' Velg riktig form av entall og flertall
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Velger riktig form, entall eller flertall, til å etterfølge et tall.
#'
#' @param x Vektor bestående av heltall.
#' @param entall `character`-verdi som angir ordet i entallsform.
#' @param flertall `character`-verdi som angir ordet i flertallsform.
#' @param nulltekst `character`-verdi som angir nullteskt. Standardverdi
#' er "0".
#'
#' @details
#' Funksjonen velger enten entalls- eller flertallsformen av et ønskelig ord,
#' hvor begge alternativer (entall og flertall) blir skrevet inn av brukeren.
#' Ordet etterfølger tallet fra heltallsvektoren som blir lest inn.
#'
#' Noen ganger er ikke standardverdien "0" som er tilegnet
#' `nulltekst`-argumentet et passende ord. Legg da inn det egnede ordet i
#' dette argumentet.
#'
#' @return
#' Gir ut en `character`-vektor. Denne vektoren består av heltallsvektoren, x,
#' som ble lest inn, hvor 0 er byttet ut med `"nulltekst"`, etterfulgt av
#' `"entall"` om heltallet foran er 1, eller `"flertall"` om heltallet foran er
#' ulik 1.
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
