# Testar berre det som skal vera ulikt Tidyverse-stilen
# Resten er testa i styler-pakken

# Sjå vignette("customizing_styler") og help(test_collection) for detaljar
# om testing av syler-funksjonar

style_text_rapwhale = purrr::partial(styler::style_text, style = rapwhale_style)

test_that("<- vert gjort om til =", {
  # <<- og := skal ikkje gjerast om
  styler:::test_collection(
    "rapwhale-style",
    "^left_assign",
    transformer = style_text_rapwhale,
    dry = "on",
    write_tree = FALSE
  ) |>
    expect_no_warning() |>
    expect_message("left_assign-in.R was identical to left_assign-out.R")
})

test_that("= vert ikkje endra", {
  styler:::test_collection(
    "rapwhale-style",
    "^equals",
    transformer = style_text_rapwhale,
    dry = "on",
    write_tree = FALSE
  ) |>
    expect_no_warning() |>
    expect_message("equals-in.R was identical to equals-out.R")
})

test_that("<- vert ikkje endra viss 'tokens' ikkje er med i 'scope'", {
  style_text_rapwhale_line_breaks = purrr::partial(
    styler::style_text,
    style = rapwhale_style,
    scope = "line_breaks"
  )

  styler:::test_collection(
    "rapwhale-style",
    "^unchanged_token",
    transformer = style_text_rapwhale_line_breaks,
    dry = "on",
    write_tree = FALSE
  ) |>
    expect_no_warning() |>
    expect_message(
      "unchanged_token-in.R was identical to unchanged_token-out.R"
    )
})

test_that("Single-line if, else, while, for og function får krøllparentesar", {
  styler:::test_collection(
    "rapwhale-style",
    "^single_line_",
    transformer = style_text_rapwhale,
    dry = "on",
    write_tree = FALSE
  ) |>
    expect_no_warning() |>
    expect_message(
      "single_line_else-in.R was identical to single_line_else-out.R"
    ) |>
    expect_message(
      "single_line_for-in.R was identical to single_line_for-out.R"
    ) |>
    expect_message(
      "single_line_function-in.R was identical to single_line_function-out.R"
    ) |>
    expect_message(
      "single_line_if-in.R was identical to single_line_if-out.R"
    ) |>
    expect_message(
      "single_line_while-in.R was identical to single_line_while-out.R"
    )
})

test_that("Korte røyr får òg linjeskift", {
  styler:::test_collection(
    "rapwhale-style",
    "^short_pipes",
    transformer = style_text_rapwhale,
    dry = "on",
    write_tree = FALSE
  ) |>
    expect_no_warning() |>
    expect_message("short_pipes-in.R was identical to short_pipes-out.R")
})
