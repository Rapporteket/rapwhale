#' Kopier LaTeX-klassefil
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Kopierer LaTeX-klassefila for kvalreg-rapportar frå rapwhale til
#' TeX Live-mappa eller anna gjeven mappe
#'
#' @param texmappe Filadresse til mappa som LaTeX-klassefila skal kopierast
#'                 til. Standard verdi = `NULL`.
#'
#' @details
#' Funksjonen kopierer LaTeX-klassefila for kvalreg-rapportar frå rapwhale til
#' ynskja mappe.
#'
#' Dersom `texmappe = NULL` vert klassefila kopiert til TeX Live-mappa
#' `C:/Users/brukarnamn/texmf/tex/latex/kvalreg/`.
#' Elles vert ho kopiert til den oppgjevne mappa `texmappe`.
#'
#' @export
#'
#' @examples
#' kopier_latex_klassefil()
#'
#' texmappe = tempdir()
#' kopier_latex_klassefil(texmappe)
kopier_latex_klassefil = function(texmappe = NULL) {

}
