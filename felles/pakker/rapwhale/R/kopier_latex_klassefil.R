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
#' Dersom mappa ikkje finst finst frå før vert ho oppretta.
#' Adressa til denne mappa vert returnert frå funksjonen usynleg.
#'
#' Dersom fila `kvalreg-rapport.cls` allereie finst i mappa det skal kopierast
#' til, vert den skriven over.
#'
#' @return Adressa til mappa som LaTeX-klassefila vert kopiert til, usynleg.
#'
#' @export
#'
#' @examples
#' kopier_latex_klassefil()
#'
#' texmappe = tempdir()
#' kopier_latex_klassefil(texmappe)
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
