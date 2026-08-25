#' Linter for FIXME-kommentarar
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Linter som sjekkar at FIXME-markørar i kommentarar
#' er skrivne med store bokstavar,
#' slik kodestilguiden vår krev.
#'
#' @details
#' Kodestilguiden seier at kommentarar om ting som må rettast opp,
#' skal starta med `FIXME`.
#' Skrivemåtar som `fixme`, `Fixme` og `!fixme` vert derfor melde som feil.
#'
#' Linteren ser på heile kommentaren,
#' ikkje berre starten av han,
#' slik at òg markørar midt i ein kommentar vert fanga opp
#' (til dømes `# Forkortingar som read_csv() brukar (fixme: utvid)`).
#'
#' Merk at linteren berre ser på *skrivemåten* av markøren.
#' Han seier ingenting om kor mange FIXME-ar ein har,
#' eller om dei bør ryddast bort.
#'
#' @return
#' Ein linter i tråd med [lintr::Linter()].
#'
#' @examples
#' lintr::lint(
#'   text = "x = 1 # fixme: rett dette",
#'   linters = fixme_stil_linter()
#' )
#'
#' # Ingen feil
#' lintr::lint(
#'   text = "x = 1 # FIXME Rett dette",
#'   linters = fixme_stil_linter()
#' )
#' @export
fixme_stil_linter = function() {
  lintr::Linter(sjekk_fixme_stil)
}

#' Sjekk skrivemåten av FIXME-markørar
#'
#' Hjelpefunksjon for [fixme_stil_linter()].
#'
#' @param source_expression
#' Uttrykk frå lintr, slik linterfunksjonar får det.
#'
#' @return
#' Ei liste med [lintr::Lint()]-objekt, eventuelt tom.
#'
#' @keywords internal
sjekk_fixme_stil = function(source_expression) {
  # Kommentarar høyrer til fila som heilskap, ikkje til enkeltuttrykk.
  if (!lintr::is_lint_level(source_expression, "file")) {
    return(list())
  }

  parsetabell = source_expression$full_parsed_content
  kommentar = parsetabell[parsetabell$token == "COMMENT", , drop = FALSE]
  if (nrow(kommentar) == 0) {
    return(list())
  }

  # Ordgrensene (\b) hindrar treff på ord som berre inneheld markøren,
  # til dømes «prefixmestring» og namnet på denne linteren sjølv.
  # Merk at linteren føreset eit UTF-8-lokale: utan det skriv parsaren
  # om «å» til teiknfølgja <U+00E5>, og då lagar «>» ei falsk ordgrense
  # framfor markøren.
  er_fixme = str_detect(kommentar$text, regex("\\bfixme\\b", ignore_case = TRUE))
  er_rett = str_detect(kommentar$text, "\\bFIXME\\b")
  ugyldige = which(er_fixme & !er_rett)

  lag_lint = function(radnr) {
    lintr::Lint(
      filename = source_expression$filename,
      line_number = kommentar$line1[radnr],
      column_number = kommentar$col1[radnr],
      type = "style",
      message = "Skriv FIXME med store bokstavar, jf. kodestilguiden.",
      line = source_expression$file_lines[[kommentar$line1[radnr]]]
    )
  }
  map(ugyldige, lag_lint)
}
