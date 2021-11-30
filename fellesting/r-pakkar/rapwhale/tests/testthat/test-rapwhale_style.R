# Testar berre det som skal vera ulikt Tidyverse-stilen
# Resten er testa i styler-pakken

# Sjå vignette("customizing_styler") og help(test_collection) for detaljar
# om testing av syler-funksjonar

style_text_rapwhale = purrr::partial(styler::style_text, style = rapwhale_style)

test_that("<- vert gjort om til =", {
  expect_warning(
    styler:::test_collection(
      "rapwhale-style",
      "^left_assign",
      transformer = style_text_rapwhale,
      dry = "on"
    ),
    NA
  )
})

test_that("= vert ikkje endra", {
  expect_warning(
    styler:::test_collection(
      "rapwhale-style",
      "^equals",
      transformer = style_text_rapwhale,
      dry = "on"
    ),
    NA
  )
})

test_that("<- vert ikkje endra viss 'tokens' ikkje er med i 'scope'", {
  style_text_rapwhale_line_breaks = purrr:::partial(
    styler::style_text,
    style = rapwhale_style,
    scope = "line_breaks"
  )

  expect_warning(
    styler:::test_collection(
      "rapwhale-style",
      "^unchanged_token",
      transformer = style_text_rapwhale_line_breaks,
      dry = "on"
    ),
    NA
  )
})

test_that("Single-line if, else, while, for og function får krøllparentesar", {
  expect_warning(
    styler:::test_collection(
      "rapwhale-style",
      "^single_line_",
      transformer = style_text_rapwhale,
      dry = "on"
    ),
    NA
  )
})
