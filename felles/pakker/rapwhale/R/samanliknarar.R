#' Samanlikn identisk
#'
#' @description Samanliknar verdiane i `verdi1` og `verdi2` elementvis, og
#'              returnerer svara i ein logisk vektor.
#'
#' @param verdi1 Vektor med verdiar som skal samanliknast mot `verdi2`.
#' @param verdi2 Vektor med verdiar som skal samanliknast mot `verdi1`.
#' @param varnamn Vektor med namn på variablar som skal samanliknast. (Ingen funksjon i denne samanliknaren)
#'
#' @details
#' Funksjonen samanliknar verdiane i `verdi1` og `verdi2` elementvis, og
#' returnerer TRUE eller FALSE for kvart par alt etter om dei er identiske
#' eller ikkje. Funksjonen handterar også NA-verdiar.
#'
#' `verdi1` og `verdi2` må ha same lengd, og utdata vil også få den same lengda.
#'
#' `varnamn` har ingen funksjon i denne samanliknaren, men er med for at
#' funksjonen skal vera på same format som andre samanliknarar.
#'
#' @return Logisk vektor som elementvis seier om `verdi1` er identisk `verdi2`.
#' @export
#'
#' @examples
#' samanlikn_identisk(
#'   varnamn = "dato_ut",
#'   verdi1 = as.Date("2020-06-07"),
#'   verdi2 = as.Date("2020-06-07")
#' )
#'
#' samanlikn_identisk(
#'   varnamn = rep("vekt", 4),
#'   verdi1 = c(74, 72, 50, NA),
#'   verdi2 = c(74, NA, 53, NA)
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

  # Utdata skal seia (elementvis) om verdi1 er lik verdi2 og skal handtera NA-verdiar
  like = (verdi1 == verdi2) | (is.na(verdi1) & is.na(verdi2))
  like[is.na(like)] = FALSE
  return(like)
}
