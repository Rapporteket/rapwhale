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

test_that("funksjonen gir forventet utdata når inndata er gruppert og ugruppert", {
  d_gruppert = tibble::tibble(
    sykehus = factor(c("B", "B", "B", "A", "A", "A", "A")),
    ki_x = c(seq(1, 7, 1)), ki_aktuell = c(rep(TRUE, 7))
  ) %>%
    dplyr::group_by(sykehus)
  d_gruppert_ut = tibble::tibble(
    sykehus = factor(c("A", "B")),
    est = c(5.5, 2), konfint_nedre = c(3.4457397432394785, -0.48413771175032977),
    konfint_ovre = c(7.5542602567605206, 4.48413771175032938), n_aktuell = c(4L, 3L)
  )
  expect_equal(aggreger_ki_snitt(d_gruppert), d_gruppert_ut)

  d_ugruppert = tibble::tibble(
    sykehus = factor(c("B", "B", "B", "A", "A", "A", "A")),
    ki_x = 1:7, ki_aktuell = rep(TRUE, 7)
  )
  d_ugruppert_ut = tibble::tibble(
    est = 4, konfint_nedre = 2.0021048397085996,
    konfint_ovre = 5.9978951602913995, n_aktuell = 7L
  )
  expect_equal(aggreger_ki_snitt(d_ugruppert), d_ugruppert_ut)
})

test_that("Funksjonen håndterer tilfeller hvor det kun er ett individ i en gruppe", {
  d_gruppe_alene = tibble::tibble(
    sykehus = factor(rep(c("A", "B"), each = 3)),
    ki_x = c(5, NA_real_, 11, 1, 2, 3),
    ki_aktuell = c(TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  ) %>%
    dplyr::group_by(sykehus)
  d_gruppe_alene_ut = tibble::tibble(
    sykehus = factor(c("A", "B")),
    est = c(5, 2), konfint_nedre = c(NA_real_, -0.48413771175032977),
    konfint_ovre = c(NA_real_, 4.48413771175032938), n_aktuell = c(1L, 3L)
  )
  expect_equal(aggreger_ki_snitt(d_gruppe_alene), d_gruppe_alene_ut)
})

test_that("Funksjonen håndterer tilfeller hvor en gruppe bare har 'ki_aktuell' som er FALSE", {
  d_gruppe_tom = tibble::tibble(
    sykehus = factor(rep(c("A", "B"), each = 3)),
    ki_x = c(1:6), ki_aktuell = c(rep(c(TRUE, FALSE), each = 3))
  ) %>%
    group_by(sykehus)
  d_gruppe_tom_ut = tibble::tibble(
    sykehus = factor(c("A", "B")),
    est = c(2, NA_real_), konfint_nedre = c(-0.48413771175032977, NA_real_),
    konfint_ovre = c(4.48413771175032938, NA_real_), n_aktuell = c(3L, 0L)
  )
  expect_equal(aggreger_ki_snitt(d_gruppe_tom), d_gruppe_tom_ut)
})

# test at n_aktuell kun teller de inkluderte fra ki_aktuell
# teste at vi får riktig feilmelding gitt antall = 1 innad i en gruppe
# teste at vi får ønsket utdata gitt at det er null observasjoner i en gruppe

# teste at ki_aktuell filtreres ut hvis NA
# teste ved standardavvik lik 0
