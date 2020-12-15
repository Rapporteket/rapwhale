#' Kjede saman element
#'
#' Tek inn ein vektor, kjedar saman elementa og returnerar
#' dei som ein tekststreng.
#'
#' @param ord Ein vektor med dei orda som skal kjedast saman.
#' @param skiljeteikn Skiljeteikn mellom orda. Standard er ", ".
#' @param og Ord som vert sett inn mellom dei to siste orda. Standard er " og ".
#' @export
#'
#' @details
#' Funksjonen tek inn ein vektor, og kjedar saman elementa med `skiljeteikn`
#' mellom kvart av dei, utanom dei to siste, som vert skilde med `og`.
#' Resultatet vert returnert som ein tekststreng med eitt element. Manglande
#' verdiar vert gjort om til teksten "NA".
#'
#' @examples
#' kjed_ord(c("Per", "Kari"))
#'
#' kjed_ord(c("Per", "Kari", "Ola"))
#'
#' kjed_ord(c("Per", "Kari", "Ola"), og = " & ")
#'
#' kjed_ord(c("Per", "Kari"), skiljeteikn = "/")
kjed_ord = function(ord, skiljeteikn = ", ", og = " og ") {
  ord = stringr::str_replace_na(ord)

  n = length(ord)

  if (n == 0) {
    tekst = character(0)
  } else if (n == 1) {
    tekst = ord
  } else if (n == 2) {
    tekst = str_flatten(ord, og)
  } else if (n > 2) {
    tekst = paste0(str_flatten(ord[1:n - 1], skiljeteikn), og, ord[n])
  }
  tekst
}
