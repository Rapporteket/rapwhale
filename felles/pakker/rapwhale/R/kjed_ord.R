#' Kople saman tekst
#'
#' Funksjon som tek inn ein vektor, kjedar saman elementa og returnerar
#' dei som ein tekststreng.
#'
#' @param ord Ein vektor med dei orda som skal kjedast saman.
#' @param skiljeteikn Skiljeteikn mellom orda. Standard er ",".
#' @param og Ord som vert sett inn mellom dei to siste orda. Standard er "og".
#' @export
#'
#' @examples
#' kjed_ord(c("Per", "Kari"))
#' kjed_ord(c("Per", "Kari", "Pål"))
kjed_ord = function(ord, skiljeteikn = ", ", og = " og ") {

  # gjør om missing til "NA" som tekststreng
  ord = stringr::str_replace_na(ord)

  # antall ord
  n = length(ord)

  # hvis det er er ingenting i objektet, returnes...ingenting
  if (n == 0) {
    tekst = paste0("")
    warning("")
  }
  if (n == 1) {
    tekst = ord
  } # Hvis det er 2 ord, bindes ordene sammen med " og ", men dette kan også endres i argumentet og.
  # F.eks om du vil bruke &-teignet
  if (n == 2) {
    tekst = str_flatten(ord, og)
  }
  # hvis det er flere enn 2 ord, bindes ordene sammen med skiljeteiknet, her komma, bortsett fra siste ord som får " og ".
  if (n > 2) {
    tekst = paste0(str_flatten(ord[1:n - 1], skiljeteikn), og, ord[n])
  }
  tekst
}
