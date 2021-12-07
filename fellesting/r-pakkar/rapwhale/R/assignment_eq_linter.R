#' @importFrom lintr Linter Lint ids_with_token with_id
NULL
#' Equals-assignment linter
#'
#' @description
#' Linter som sjekkar at `=` alltid vert brukt ved tilordning.
assignment_eq_linter = function() {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, "LEFT_ASSIGN"),
      function(id) {
        parsed = with_id(source_file, id)
        Lint(
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
