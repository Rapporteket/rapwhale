# Aggregering - Beregn snitt -------------------------------------------------------

context("aggreger_ki_snitt")

test_that("Feilmelding hvis ikke tibble/data.frame med nødvendige kolonner", {
  d_uten_aktuell = tibble::tibble(foo = 1:3, ki_x = rep(TRUE, 3))
  d_uten_x = tibble::tibble(foo = 1:3, ki_aktuell = rep(TRUE, 3))
  d_uten_begge = tibble::tibble(foo = 1:3)
  liste = list(ki_x = c(FALSE, TRUE, TRUE), ki_aktuell = c(TRUE, TRUE, TRUE))

  feilmelding_kol = "Inndata må være tibble/data.frame med kolonnene 'ki_x' og 'ki_aktuell'"
  expect_error(aggreger_ki_snitt(liste), feilmelding_kol)
  expect_error(aggreger_ki_snitt(d_uten_aktuell), feilmelding_kol)
  expect_error(aggreger_ki_snitt(d_uten_x), feilmelding_kol)
  expect_error(aggreger_ki_snitt(d_uten_begge), feilmelding_kol)
})

# test for feil variabeltyper
test_that("Feilmelding hvis data av feil type", {
  d_feil_aktuell_tekst = tibble::tibble(ki_x = c(15, 12.2, 12.5), ki_aktuell = c("0", "1", "1"))
  d_feil_aktuell_fak = tibble::tibble(ki_x = c(15, 12.2, 12.2), ki_aktuell = c(factor(c("5", "5", "5"))))
  d_feil_aktuell_num = tibble::tibble(ki_x = c(13, 14, 16), ki_aktuell = c(TRUE, TRUE, 2))

  feilmelding_aktuell = "'ki_aktuell' må være boolsk"
  expect_error(aggreger_ki_snitt(d_feil_aktuell_tekst), feilmelding_aktuell)
  expect_error(aggreger_ki_snitt(d_feil_aktuell_num), feilmelding_aktuell)
  expect_error(aggreger_ki_snitt(d_feil_aktuell_fak), feilmelding_aktuell)


  d_feil_x_tekst = tibble::tibble(ki_x = c("0", "1", "1"), ki_aktuell = c(FALSE, TRUE, TRUE))
  d_feil_x_fak = tibble::tibble(ki_x = factor(c("5", "5", "5")), ki_aktuell = c(FALSE, TRUE, TRUE))
  d_feil_x_lgl = tibble::tibble(ki_x = c(FALSE, TRUE, TRUE), ki_aktuell = c(TRUE, TRUE, TRUE))

  feilmelding_x = "'ki_x' må være numerisk"
  expect_error(aggreger_ki_snitt(d_feil_x_tekst), feilmelding_x)
  expect_error(aggreger_ki_snitt(d_feil_x_fak), feilmelding_x)
  expect_error(aggreger_ki_snitt(d_feil_x_lgl), feilmelding_x)
})

test_that("Feilmelding hvis 'ki_x' er missing når 'ki_aktuell' er TRUE", {
  d_x_na = tibble::tibble(ki_x = c(15, 12, NA_real_), ki_aktuell = c(TRUE, TRUE, TRUE))

  feilmelding_x_na = "'ki_x' må være en numerisk verdi hvis 'ki_aktuell' er TRUE"
  expect_error(aggreger_ki_snitt(d_x_na), feilmelding_x_na)
})
