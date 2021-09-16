# Testing av expand_soyle() og expand_soyle_str_fig()

test_that("expand_soyle() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.05, 0.00)
  expect_identical(expand_soyle(), riktig_tallvektor)
})

test_that("expand_soyle_str_fig() gir ut riktig tallvektor", {
  riktig_tallvektor = c(0.00, 0.00, 0.09, 0.00)
  expect_identical(expand_soyle_str_fig(), riktig_tallvektor)
})
