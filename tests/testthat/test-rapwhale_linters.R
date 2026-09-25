test_that("rapwhale_linters() gjev ei namngjeven liste med linterar", {
  linterar = rapwhale_linters()
  expect_type(linterar, "list")
  expect_true(all(purrr::map_lgl(linterar, inherits, what = "linter")))
  expect_named(linterar)
  expect_true(all(nzchar(names(linterar), keepNA = TRUE)))
})

test_that("rapwhale_linters() tek med FIXME-linteren vår", {
  expect_true("fixme_stil_linter" %in% names(rapwhale_linters()))
})

test_that("rapwhale_linters() slår av linterar som kodestilguiden ikkje brukar", {
  avslegne = c(
    "object_usage_linter", "indentation_linter", "cyclocomp_linter",
    "implicit_integer_linter", "nonportable_path_linter",
    "todo_comment_linter", "undesirable_operator_linter"
  )
  expect_false(any(avslegne %in% names(rapwhale_linters())))
})

test_that("rapwhale_linters() krev = for tilordning", {
  linter = rapwhale_linters()[["assignment_linter"]]
  lintr::expect_no_lint("x = 1", linter)
  lintr::expect_lint("x <- 1", "not <-", linter)
})

test_that("rapwhale_linters() tilrår tidyverse-funksjonar", {
  linter = rapwhale_linters()[["undesirable_function_linter"]]
  lintr::expect_lint("y = lapply(x, f)", "map\\(\\)", linter)
  lintr::expect_lint("y = ifelse(x, 1, 2)", "if_else\\(\\)", linter)
})

test_that("rapwhale_linters() tillèt at linterar vert overstyrte", {
  linterar = rapwhale_linters(
    lintr::line_length_linter(80),
    undesirable_function_linter = NULL
  )
  expect_false("undesirable_function_linter" %in% names(linterar))
  lintr::expect_lint(strrep("a", 90), "80 characters", linterar[["line_length_linter"]])
  lintr::expect_no_lint(strrep("a", 90), rapwhale_linters()[["line_length_linter"]])
})

test_that("rapwhale_linters() tillèt at linterar vert lagde til", {
  ekstra = lintr::Linter(function(source_expression) list(), name = "ekstra_linter")
  expect_true("ekstra_linter" %in% names(rapwhale_linters(ekstra_linter = ekstra)))
})
