#' Equals-assignment linter
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Linter som sjekkar at `=` alltid vert brukt ved tilordning.
#'
#' @keywords internal
assignment_eq_linter = function() {
  lintr::Linter(function(source_file) {
    lapply(
      lintr::ids_with_token(source_file, "LEFT_ASSIGN"),
      function(id) {
        parsed = lintr::with_id(source_file, id)
        lintr::Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Use =, not <-, for assignment.",
          line = source_file$lines[as.character(parsed$line1)]
        )
      }
    )
  })
}
