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
  # Kommentarar høyrer til fila som heilskap, ikkje til enkeltuttrykk,
  # så linteren skal berre kallast på filnivå.
  lintr::Linter(sjekk_fixme_stil, linter_level = "file")
}

#' Sjekk skrivemåten av FIXME-markørar
#'
#' Hjelpefunksjon for [fixme_stil_linter()].
#'
#' @param source_expression
#' Uttrykk frå lintr, på filnivå.
#' [fixme_stil_linter()] registrerer funksjonen med
#' `linter_level = "file"`, så lintr kallar han berre med
#' uttrykk som har `full_parsed_content` for heile fila.
#'
#' @return
#' Ei liste med [lintr::Lint()]-objekt, eventuelt tom.
#'
#' @keywords internal
sjekk_fixme_stil = function(source_expression) {
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
  er_fixme_markorar = str_locate_all(kommentar$text, regex("\\bfixme\\b", ignore_case = TRUE))

  # Posisjonen til den første markøren i kommentaren som ikkje alt er
  # skriven med store bokstavar, eller NA om kommentaren er i orden.
  # Ein rett markør skal ikkje skjula ein feil markør seinare i same
  # kommentaren.
  finn_forste_feil = function(radnr) {
    posisjon = er_fixme_markorar[[radnr]]
    skrivemate = str_sub(kommentar$text[radnr], posisjon[, "start"], posisjon[, "end"])
    feil = which(skrivemate != "FIXME")
    if (length(feil) == 0) {
      NA_integer_
    } else {
      posisjon[feil[1], "start"]
    }
  }
  forste_feil = map_int(seq_len(nrow(kommentar)), finn_forste_feil)
  ugyldige = which(!is.na(forste_feil))

  lag_lint = function(radnr) {
    linjenummer = kommentar$line1[radnr]
    linje = source_expression$file_lines[[linjenummer]]

    # Kolonnane i parsetabellen er byteposisjonar, medan lintr::Lint()
    # ventar teiknposisjonar. Ein kommentar går alltid ut linja, så me
    # finn starten hans ved å telja teikn bakfrå i staden for å bruka
    # col1. Elles ville kolonnen peika for langt til høgre på linjer med
    # æ, ø og å, og Lint() ville stoppa med feil dersom byteposisjonen
    # kom forbi nchar(linje) + 1.
    kommentarstart = nchar(linje) - nchar(kommentar$text[radnr]) + 1L
    kolonne = kommentarstart + forste_feil[radnr] - 1L

    lintr::Lint(
      filename = source_expression$filename,
      line_number = linjenummer,
      column_number = kolonne,
      type = "style",
      message = "Skriv FIXME med store bokstavar, jf. kodestilguiden.",
      line = linje,
      # «fixme» er alltid fem teikn, så me kan streka under heile markøren.
      ranges = list(c(kolonne, kolonne + 4L))
    )
  }
  map(ugyldige, lag_lint)
}
