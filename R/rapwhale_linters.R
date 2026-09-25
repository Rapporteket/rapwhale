#' Standardlinterar
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Gjev linterane som svarar til kodestilguiden vår,
#' slik at alle pakkane på Rapporteket kan bruka same reglar.
#'
#' @details
#' Funksjonen er meint for bruk i `.lintr`-filer:
#'
#' ```
#' linters: rapwhale::rapwhale_linters()
#' ```
#'
#' Utgangspunktet er alle linterane i lintr-pakken
#' ([lintr::all_linters()]),
#' med nokre slegne av og nokre tilpassa kodestilguiden vår,
#' i tillegg til [fixme_stil_linter()].
#'
#' Treng ein pakke unntak frå standarden,
#' kan ein leggja dei til som argument,
#' slik at det går tydeleg fram kva som er annleis:
#'
#' ```
#' linters: rapwhale::rapwhale_linters(
#'     line_length_linter(100),
#'     undesirable_function_linter = NULL
#'   )
#' ```
#'
#' @param ...
#' Linterar som skal leggjast til eller erstatta standardlinterane,
#' til dømes `lintr::line_length_linter(100)`,
#' eller `namn_linter = NULL` for å slå av ein linter.
#' Sjå [lintr::modify_defaults()].
#'
#' @return
#' Ei namngjeven liste med linterar,
#' til bruk i `.lintr`-filer eller i [lintr::lint_package()].
#'
#' @examples
#' # Standardlinterane
#' names(rapwhale_linters())
#'
#' # Med kortare linjer og utan linteren for uønskte funksjonar
#' lintr::lint(
#'   text = "x = lapply(1:3, sqrt)",
#'   linters = rapwhale_linters(
#'     lintr::line_length_linter(80),
#'     undesirable_function_linter = NULL
#'   )
#' )
#' @export
rapwhale_linters = function(...) {
  standard = lintr::all_linters(
    lintr::assignment_linter(operator = c("=", "<<-")),
    object_usage_linter = NULL,
    indentation_linter = NULL,
    cyclocomp_linter = NULL,
    lintr::line_length_linter(120),
    lintr::object_length_linter(40),
    implicit_integer_linter = NULL,
    nonportable_path_linter = NULL,
    todo_comment_linter = NULL,
    undesirable_operator_linter = NULL,
    lintr::undesirable_function_linter(
      fun = lintr::modify_defaults(
        defaults = lintr::default_undesirable_functions,
        data.frame = "use tibble() instead",
        rbind      = "use bind_rows() instead",
        subset     = "use filter() instead",
        lapply     = "use map() instead",
        ifelse     = "use if_else() instead",
        merge      = "use left_join()/inner_join() instead",
        substr     = "use str_sub() instead",
        toupper    = "use str_to_upper() instead",
        tolower    = "use str_to_lower() instead"
      )
    ),
    fixme_stil_linter()
  )
  lintr::modify_defaults(defaults = standard, ...)
}
