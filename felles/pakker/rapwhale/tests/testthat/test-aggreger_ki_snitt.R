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
  d_feil_aktuell_na = tibble::tibble(ki_x = 1:6, ki_aktuell = c(TRUE, TRUE, FALSE, FALSE, NA, TRUE))

  feilmelding_aktuell = "'ki_aktuell' må være TRUE eller FALSE"
  expect_error(aggreger_ki_snitt(d_feil_aktuell_tekst), feilmelding_aktuell)
  expect_error(aggreger_ki_snitt(d_feil_aktuell_num), feilmelding_aktuell)
  expect_error(aggreger_ki_snitt(d_feil_aktuell_fak), feilmelding_aktuell)
  expect_error(aggreger_ki_snitt(d_feil_aktuell_na), feilmelding_aktuell)

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

test_that("Feilmelding hvis 'alfa' ikke er et tall mellom 0 og 1", {
  d_test = tibble::tibble(
    ki_x = c(15, 12, 12),
    ki_aktuell = c(TRUE, TRUE, TRUE)
  )
  feilmelding_alfa = "'alfa' må være et tall mellom 0 og 1"
  expect_error(aggreger_ki_snitt(d_test, alfa = 0), feilmelding_alfa)
  expect_error(aggreger_ki_snitt(d_test, alfa = 1), feilmelding_alfa)
  expect_error(aggreger_ki_snitt(d_test, alfa = 1.2), feilmelding_alfa)
  expect_error(aggreger_ki_snitt(d_test, alfa = "0.1"), feilmelding_alfa)
})

test_that("Forventet utdata når inndata er gruppert og ugruppert", {
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

test_that("Forventet utdata når alfa endres fra standard", {
  d_test = tibble::tibble(
    sykehus = factor(c("B", "B", "B", "A", "A", "A", "A")),
    ki_x = c(1, 2, 3, 4, 5, 6, 8), ki_aktuell = rep(TRUE, 7)
  )
  d_test_ut = tibble::tibble(
    est = 4.14285714285714324, konfint_nedre = 2.37260820476272194,
    konfint_ovre = 5.91310608095156365, n_aktuell = 7L
  )

  expect_equal(aggreger_ki_snitt(d_test, alfa = 0.1), d_test_ut)
})

test_that("Funksjonen gir alltid ut ugrupperte data", {
  d_test = tibble::tibble(
    sjukehus = factor(c("A", "B", "B")),
    post = factor(c("1", "1", "2")),
    ki_x = c(1, 5, 8),
    ki_aktuell = c(TRUE, TRUE, TRUE)
  )
  expect_length(group_vars(aggreger_ki_snitt(group_by(d_test, sjukehus))), 0)
  expect_length(group_vars(aggreger_ki_snitt(group_by(d_test, sjukehus, post))), 0)
})

test_that("Tilfeller hvor det kun er ett individ i en gruppe", {
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

test_that("Tilfeller hvor en gruppe bare har 'ki_aktuell' som er FALSE", {
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

test_that("Tilfeller hvor standardavvik er 0", {
  d_sd_lik_null = tibble::tibble(
    sykehus = factor(rep(c("A", "B"), each = 3)),
    ki_x = c(rep(3, 3), 4, 5, 6),
    ki_aktuell = rep(TRUE, 6)
  ) %>%
    group_by(sykehus)
  d_sd_lik_null_ut = tibble::tibble(
    sykehus = factor(c("A", "B")),
    est = c(3, 5),
    konfint_nedre = c(NA_real_, 2.5158622882496702),
    konfint_ovre = c(NA_real_, 7.4841377117503303),
    n_aktuell = c(3L, 3L)
  )

  expect_equal(aggreger_ki_snitt(d_sd_lik_null), d_sd_lik_null_ut)
})

test_that("'ki_aktuell' inkluderes i 'n_aktuell' kun når den er TRUE", {
  d_ki_akt_variert = tibble::tibble(
    ki_x = 1:6,
    ki_aktuell = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  d_ki_akt_variert_ut = tibble::tibble(
    est = 3.25,
    konfint_nedre = 0.53246911620398474,
    konfint_ovre = 5.96753088379601504,
    n_aktuell = 4L
  )
  expect_equal(aggreger_ki_snitt(d_ki_akt_variert), d_ki_akt_variert_ut)
})
