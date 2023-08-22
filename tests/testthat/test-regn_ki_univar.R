# Lag datasett for bruk i flere tester
x = c(2, 2, 3, 1, 4, 3, 2, 2, 1, 3)
x_snitt = mean(x)

test_that("Gir riktige verdier (med valg av alfa, uten bootstrap)", {
  fasit_05 = tibble::tibble(
    low = 1.6213528511605381777,
    mean = c("mean of x" = x_snitt),
    high = 2.978647148839461245
  )
  fasit_10 = tibble::tibble(
    low = 1.7500661202031289054,
    mean = c("mean of x" = x_snitt),
    high = 2.8499338797968705173
  )

  expect_identical(regn_ki_univar(x), fasit_05)
  expect_identical(regn_ki_univar(x, alfa = 0.1), fasit_10)
})

