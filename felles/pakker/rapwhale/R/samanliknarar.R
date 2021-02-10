#' Undersøk om verdiar i vektorar er identiske
#'
#' @description Samanliknar elementvis om verdiane i to vektorar er identiske
#'              (inkludert tilfellet der begge er `NA`). Er meint å brukast
#'              saman med [analyser_valideringsdatasett()]
#'
#' @param verdi1 Vektor med verdiar av valfri type.
#' @param verdi2 Vektor med verdiar av same type som `verdi1`.
#' @param varnamn Vektor med namn på variablane som skal samanliknast.
#'   Må ha same lengd som `verdi1` og `verdi2` eller vera `NULL`.
#'
#' @details
#' Funksjonen samanliknar verdiane i `verdi1` og `verdi2` elementvis og
#' returnerer ein logisk vektor som seier om dei er identiske.
#' Fungerer i praksis som `==`, men med den forskjellen at to `NA`-verdiar
#' òg vert rekna som identiske.
#'
#' Argumentet `varnamn` har ingen verknad, men er med for at funksjonen
#' skal vera på same format som andre samanliknarar, for kompatibilitet
#' med [analyser_valideringsdatasett()].
#'
#' @return Logisk vektor som elementvis seier om `verdi1` er identisk
#'   med `verdi2` (`TRUE`) eller ikkje (`FALSE`). Vil aldri innehalda
#'   `NA`-verdiar.
#' @export
#'
#' @examples
#' # Samanlikn NA-handteringa med den i «==»
#' x1 = c(5, 7, NA, NA)
#' x2 = c(5, 8, NA, 5)
#' samanlikn_identisk(x1, x2)
#' x1 == x2
#' #
#' # Handterer verdiar av valfri type/klasse
#' samanlikn_identisk(
#'   verdi1 = as.Date(c("1999-05-17", "1999-12-24")),
#'   verdi2 = as.Date(c("1999-05-17", "2000-12-24")),
#'   varnamn = c("dato_ut", "dato_ut")
#' )
samanlikn_identisk = function(verdi1, verdi2, varnamn = NULL) {
  if (!(typeof(verdi1) == typeof(verdi2))) {
    stop("«verdi1» og «verdi2» må vera av same type")
  }
  if (!((is.character(varnamn) || is.null(varnamn)))) {
    stop("«varnamn» må vera tekstvektor eller NULL")
  }
  if (!((length(verdi1) == length(verdi2) &&
    (is.null(varnamn) || length(verdi1) == length(varnamn))
  ))) {
    stop("Argumenta må vera vektorar av same lengd")
  }

  like = (verdi1 == verdi2) | (is.na(verdi1) & is.na(verdi2))
  like[is.na(like)] = FALSE
  like
}
