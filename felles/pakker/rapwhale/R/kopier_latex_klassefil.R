#' Kopier LaTeX-klassefil til TeX Live eller liknande
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Kopierer LaTeX-klassefila for kvalreg-rapportar frå rapwhale til
#' TeX Live-mappa eller anna gjeven mappe.
#'
#' @param texmappe Adressa til mappa som LaTeX-klassefila skal kopierast
#'                 til. Viss `NULL` (standard), vert fila kopiert til
#'                 ei høveleg undermappe i TeX Live-mappa.
#'
#' @details
#' Dersom `texmappe` er `NULL`,
#' vert klassefila kopiert til undermappa
#' `/tex/latex/kvalreg/` i TeX Live-mappa
#' definert av TeX-variabelen `TEXMFHOME`.
#' Det vert typisk noko slikt:
#' `C:/Users/brukarnamn/texmf/tex/latex/kvalreg/`.
#' Dette gjer at ho automatisk vert funnen av LuaLaTeX.
#' Elles vert ho kopiert til den oppgjevne mappa `texmappe`.
#'
#' Dersom mappa ikkje finst finst frå før, vert ho oppretta.
#' Dersom fila `kvalreg-rapport.cls` allereie finst i mappa det skal kopierast
#' til, vert ho skriven over.
#'
#' @return Adressa til mappa som LaTeX-klassefila vart kopiert til, usynleg.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kopier_latex_klassefil()
#'
#' texmappe = tempdir()
#' kopier_latex_klassefil(texmappe)
#' }
kopier_latex_klassefil = function(texmappe = NULL) {
  if (is.null(texmappe)) {
    texmappe_rot = system2("kpsewhich", "-var-value=TEXMFHOME", stdout = TRUE)
    texmappe = paste0(texmappe_rot, "/tex/latex/kvalreg/")
  }

  dir.create(texmappe, showWarnings = FALSE, recursive = TRUE)

  klassefil_adresse = system.file("extdata", "kvalreg-rapport.cls",
    package = "rapwhale"
  )

  file.copy(
    from = klassefil_adresse, to = texmappe,
    overwrite = TRUE, copy.date = TRUE
  )

  invisible(texmappe)
}
