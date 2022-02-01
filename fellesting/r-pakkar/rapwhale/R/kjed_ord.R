#' Kjed saman elementa i ein vektor til ein tekststreng
#'
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Tek inn ein vektor og kjedar saman (listar opp) elementa.
#' Returnerer resultatet som ein tekststreng.
#'
#' @param ord Ein vektor med dei elementa som skal kjedast saman.
#' @param skiljeteikn Skiljeteikn som skal brukast mellom alle
#'   elementa (utanom dei to siste).
#' @param og Tekst som skal setjast inn mellom dei to siste elementa.
#'
#' @details
#' Funksjonen er meint å brukast til å laga ei opplisting av
#' elementa i `ord`
#' (typisk ein tekst- eller talvektor)
#' på ein naturleg måte.
#'
#' Funksjonen tek inn ein vektor og kjedar saman elementa med `skiljeteikn`
#' (som standard eit komma)
#' mellom kvart element, utanom dei to siste, som vert kjeda saman med `og`
#' (som standard ordet «og» med mellomrom rundt).
#' Resultatet vert returnert som ein tekststreng med eitt element.
#'
#' Funksjonen handterar ulike typar inndata, som tekst, tal, dato
#' og andre typar objekt der [base::as.character()] er definert.
#' Manglande verdiar vert gjort om til teksten `"NA"`.
#'
#' @return Ein tekststreng med eitt element,
#'   der alle elementa frå `ord` er kjeda saman.
#'
#' @export
#'
#' @examples
#' kjed_ord(c("Per", "Kari"))
#'
#' kjed_ord(c("Per", "Kari", "Ola"))
#'
#' kjed_ord(c("Per", "Kari", "Ola"), og = " & ")
#'
#' kjed_ord(c(
#'   "Førde", "Bergen og Voss",
#'   "Oslo", "Stavanger"
#' ),
#' skiljeteikn = "; ", og = " – og til slutt "
#' )
kjed_ord = function(ord, skiljeteikn = ", ", og = " og ") {
  ord = stringr::str_replace_na(ord)

  n = length(ord)

  if (n == 0) {
    tekst = character(0)
  } else if (n == 1) {
    tekst = ord
  } else if (n == 2) {
    tekst = paste0(ord, collapse = og)
  } else if (n > 2) {
    tekst = paste0(
      paste0(ord[1:(n - 1)], collapse = skiljeteikn),
      og, ord[n]
    )
  }
  tekst
}
