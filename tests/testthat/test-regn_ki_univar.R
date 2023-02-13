test_that("Gir NA-verdier i ki om for få elementer eller for lav varians", {
  x_faa_element = 1
  x_lav_varians = rep(1, 4)

  fasit = tibble::tibble(low = NA_real_, mean = 1, high = NA_real_)

  expect_identical(regn_ki_univar(x_faa_element), fasit)
  expect_identical(regn_ki_univar(x_lav_varians), fasit)
})

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

