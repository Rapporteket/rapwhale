# Aggregering - Beregn rater ----------------------------------------------


# Feilmeldinger -----------------------------------------------------------

test_that("Feilmelding hvis ikke tibble/data.frame med nødvendige kolonner", {
  d = tibble(
    ki_antall = 1,
    ki_eksponering = 1,
    ki_aktuell = TRUE
  )

  feilmelding_kol = paste0(
    "Inndata må være tibble/data.frame med kolonnene «ki_antall», ",
    "«ki_eksponering» og «ki_aktuell»"
  )

  expect_error(aggreger_ki_rate(select(d, -ki_antall)), feilmelding_kol)
  expect_error(aggreger_ki_rate(select(d, -ki_eksponering)), feilmelding_kol)
  expect_error(aggreger_ki_rate(select(d, -ki_aktuell)), feilmelding_kol)
  expect_error(aggreger_ki_rate(unclass(d)), feilmelding_kol)
})

test_that("Feilmelding hvis data av feil type", {
  d_feil_antall_tekst = tibble(
    ki_antall = c("5", "2", "7"),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_tekst = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c("100", "95", "101"),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_tekst = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c("TRUE", "TRUE", "FALSE")
  )
  d_feil_antall_fak = tibble(
    ki_antall = factor(c(5, 2, 7)),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_fak = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = factor(c(100, 95, 101)),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_fak = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = factor(c(TRUE, TRUE, FALSE))
  )
  d_feil_antall_log = tibble(
    ki_antall = c(TRUE, TRUE, FALSE),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_log = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(TRUE, TRUE, FALSE),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_num = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(1, 1, 0)
  )

  feilmelding = paste0(
    "Kriterievariablen «ki_antall» og «ki_eksponering» må være numerisk, ",
    "mens «ki_aktuell» må være logisk"
  )

  expect_error(aggreger_ki_rate(d_feil_antall_tekst, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_eksponering_tekst, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_aktuell_tekst, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_antall_fak, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_eksponering_fak, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_aktuell_fak, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_antall_log, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_eksponering_log, feilmelding))
  expect_error(aggreger_ki_rate(d_feil_aktuell_num, feilmelding))
})

test_that("Feilmelding hvis ki_aktuell inneholder annet enn TRUE eller FALSE", {
  d_aktuell_ikke_ok = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, NA, FALSE)
  )

  feilmelding = "«ki_aktuell» må være TRUE eller FALSE"

  expect_error(aggreger_ki_rate(d_aktuell_ikke_ok), feilmelding)
})

test_that("Feilmelding hvis ki_antall eller ki_eksponering er missing dersom ki_aktuell er TRUE", {
  d_antall_ikke_ok = tibble(
    ki_antall = c(5, NA_real_, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_eksponering_ikke_ok = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, NA_real_, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding_antall_ikke_ok =
    "«ki_antall» kan ikke være NA om «ki_aktuell» er TRUE"
  feilmelding_eksponering_ikke_ok =
    "«ki_eksponering» kan ikke være NA om «ki_aktuell» er TRUE"

  expect_error(
    aggreger_ki_rate(d_antall_ikke_ok),
    feilmelding_antall_ikke_ok
  )
  expect_error(
    aggreger_ki_rate(d_eksponering_ikke_ok),
    feilmelding_eksponering_ikke_ok
  )
})

test_that("Feilmelding hvis minst en ki_antall er mindre enn 0", {
  d_antall_lavere_0 = tibble(
    ki_antall = c(5, -1, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding = "«ki_antall» kan ikke være mindre enn 0"

  expect_error(aggreger_ki_rate(d_antall_lavere_0), feilmelding)
})

test_that("Feilmelding hvis minst en ki_eksponering er mindre enn eller lik 0", {
  d_eksponering_lavere_0 = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, -1, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_eksponering_lik_0 = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 0, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding = "«ki_eksponering» kan ikke være mindre enn eller lik 0"

  expect_error(aggreger_ki_rate(d_eksponering_lavere_0), feilmelding)
  expect_error(aggreger_ki_rate(d_eksponering_lik_0), feilmelding)
})

test_that("Feilmelding hvis konf_niva ikke er et tall mellom 0 og 1", {
  d_var_ok = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding_konf_niva = "«konf_niva» må være et tall mellom 0 og 1"

  expect_error(aggreger_ki_rate(d_var_ok, konf_niva = 1.2), feilmelding_konf_niva)
  expect_error(aggreger_ki_rate(d_var_ok, konf_niva = 0), feilmelding_konf_niva)
  expect_error(aggreger_ki_rate(d_var_ok, konf_niva = 1), feilmelding_konf_niva)
  expect_error(aggreger_ki_rate(d_var_ok, konf_niva = "0.95"), feilmelding_konf_niva)
})

test_that("Feilmelding hvis multiplikator ikke er et positivt heltall", {
  d_var_ok = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding_multiplikator = "«multiplikator» må være et positivt tall"

  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = 0),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = -1),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = NA_real_),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = "1000"),
    feilmelding_multiplikator
  )
})

test_that("Feilmelding hvis konf_niva eller multiplikator ikke har lengde 1", {
  d_var_ok = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding_konf_niva = "«konf_niva» må ha lengde 1"
  feilmelding_multiplikator = "«multiplikator» må ha lengde 1"

  expect_error(
    aggreger_ki_rate(d_var_ok, konf_niva = NULL),
    feilmelding_konf_niva
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, konf_niva = c(0.95, 0.9)),
    feilmelding_konf_niva
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = NULL),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = c(1000, 100)),
    feilmelding_multiplikator
  )
})


# Tester med NA og antall = 0 ---------------------------------------------

test_that("aggreger_ki_rate() fungerer (uten feilmelding) om ki_aktuell er FALSE (og ellers er gyldig)", {
  d_antall_ok_men_na = tibble(
    ki_antall = c(5, 2, NA),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_ok_man_na = tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  d_eksponering_ok_men_na = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, NA),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_eksponering_ok_man_na = tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  expect_equal(
    object = aggreger_ki_rate(d_antall_ok_men_na),
    expected = svar_antall_ok_man_na,
    tolerance = testthat_tolerance()
  )
  expect_equal(
    object = aggreger_ki_rate(d_eksponering_ok_men_na),
    expected = svar_eksponering_ok_man_na,
    tolerance = testthat_tolerance()
  )
})

test_that("Funksjonen tillater tilfeller hvor antall observasjoner er 0", {
  d_ugruppert = tibble(
    ki_antall = rep(0, 3),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_ugruppert = tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.015362729607969183993
  )

  d_gruppert = tibble(
    sykehus = factor(rep(c("B", "A"), each = 3)),
    ki_antall = rep(0, 6),
    ki_eksponering = c(100, 95, 101, 200, 190, 202),
    ki_aktuell = c(TRUE, TRUE, FALSE, rep(FALSE, 3))
  ) |>
    group_by(sykehus)
  svar_gruppert = tibble(
    sykehus = factor(c("A", "B")),
    est = c(NA_real_, 0),
    konfint_nedre = c(NA_real_, 0),
    konfint_ovre = c(NA_real_, 0.015362729607969183993)
  )

  expect_equal(aggreger_ki_rate(d_ugruppert), svar_ugruppert, tolerance = 1e-15)
  expect_equal(aggreger_ki_rate(d_gruppert), svar_gruppert, tolerance = 1e-15)
})


# Grupperingstester -------------------------------------------------------

test_that("Funksjonen returnerer «NA» for de grupperte verdiene som ikke har noen øvrig gruppetilhørighet", {
  d_gruppert_med_na = suppressWarnings({
    tibble(
      sykehus = factor(rep(c("B", "A", NA), each = 3)),
      ki_antall = c(rep(0, 6), 5, 2, 7),
      ki_eksponering = c(100, 95, 101, 200, 190, 202, 50, 47, 51),
      ki_aktuell = c(rep(c(TRUE, FALSE), each = 3), FALSE, TRUE, FALSE)
    ) |>
      group_by(sykehus)
  })
  svar_gruppert_med_na = tibble(
    sykehus = factor(c("A", "B", NA)),
    est = c(NA_real_, 0, 2 / 47),
    konfint_nedre = c(
      NA_real_,
      0,
      0.0070736063379283550812
    ),
    konfint_ovre = c(
      NA_real_,
      0.010120717140385104144,
      0.13135622846119235363
    )
  )

  expect_equal(aggreger_ki_rate(d_gruppert_med_na),
    expected = svar_gruppert_med_na,
    tolerance = 1e-15
  )
})

test_that(paste0(
  "Funksjonen gir en advarsel når det finnes ubrukte nivå i ",
  "grupperingsvariabel (men likevel en rad for hvert *mulige* nivå)"
), {
  d_gruppert_ekstra_levels = tibble(
    sykehus = factor(rep(c("B", "A"), each = 3), levels = LETTERS[1:4]),
    ki_antall = c(rep(0, 6)),
    ki_eksponering = c(100, 95, 101, 200, 190, 202),
    ki_aktuell = c(rep(c(TRUE, FALSE), each = 3))
  ) |>
    group_by(sykehus, .drop = FALSE)
  svar_gruppert_ekstra_levels = tibble(
    sykehus = factor(LETTERS[1:4], levels = LETTERS[1:4]),
    est = c(NA_real_, 0, NA_real_, NA_real_),
    konfint_nedre = c(NA_real_, 0, NA_real_, NA_real_),
    konfint_ovre = c(
      NA_real_,
      0.010120717140385100674,
      NA_real_,
      NA_real_
    )
  )

  feilmelding_ekstra_levels = "Det finnes grupper uten observasjoner i grupperingsvariabel"

  expect_warning(aggreger_ki_rate(d_gruppert_ekstra_levels),
    regexp = feilmelding_ekstra_levels
  )
  expect_equal(suppressWarnings(aggreger_ki_rate(d_gruppert_ekstra_levels)),
    expected = svar_gruppert_ekstra_levels,
    tolerance = testthat_tolerance()
  )
})

test_that(paste0(
  "Funksjonen returnerer en tom ugruppert tibble med riktige ",
  "kolonner hvis inndata er gruppert med null rader"
), {
  d_gruppert_tom = tibble(
    sykehus = factor(),
    ki_antall = numeric(),
    ki_eksponering = numeric(),
    ki_aktuell = logical()
  ) |>
    group_by(sykehus)
  svar_tom = tibble(
    sykehus = factor(),
    est = numeric(),
    konfint_nedre = numeric(),
    konfint_ovre = numeric()
  )

  expect_identical(aggreger_ki_rate(d_gruppert_tom), svar_tom)
})


# Rigktig resultat --------------------------------------------------------

test_that("Funksjonen gir ut forventet resultat", {
  d_ingen = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = rep(FALSE, 3)
  )
  svar_ingen = tibble(
    est = NA_real_,
    konfint_nedre = NA_real_,
    konfint_ovre = NA_real_
  )

  d_alle = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = rep(TRUE, 3)
  )
  svar_alle = tibble(
    est = 0.047297297297297299923,
    konfint_nedre = 0.026644437703193836475,
    konfint_ovre = 0.076570304030960267827
  )

  svar_alle_multiplikator_1000 = svar_alle * 1000

  d_noen = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_noen = tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  expect_equal(aggreger_ki_rate(d_ingen),
    expected = svar_ingen,
    tolerance = testthat_tolerance()
  )
  expect_equal(aggreger_ki_rate(d_alle),
    expected = svar_alle,
    tolerance = testthat_tolerance()
  )
  expect_equal(aggreger_ki_rate(d_noen),
    expected = svar_noen,
    tolerance = testthat_tolerance()
  )
})

test_that("Funksjonen støtter angivelse av konfidensinvå", {
  d_antall_lik_0 = tibble(
    ki_antall = rep(0, 3),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_lik_0_95 = tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.015362729607969183993
  )
  svar_antall_lik_0_90 = tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.011808128682020746503
  )

  d_antall_ulik_0 = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_ulik_0_95 = tibble(
    est = 7 / 195,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )
  svar_antall_ulik_0_90 = tibble(
    est = 7 / 195,
    konfint_nedre = 0.017943117460240590177,
    konfint_ovre = 0.063051223283699317501
  )

  expect_equal(aggreger_ki_rate(d_antall_lik_0),
    expected = svar_antall_lik_0_95,
    tolerance = 1e-15
  ) # Standard skal være 95 %-KI
  expect_equal(aggreger_ki_rate(d_antall_lik_0, konf_niva = 0.95),
    expected = svar_antall_lik_0_95,
    tolerance = 1e-15
  )
  expect_equal(aggreger_ki_rate(d_antall_lik_0, konf_niva = 0.9),
    expected = svar_antall_lik_0_90,
    tolerance = 1e-15
  )
  expect_equal(aggreger_ki_rate(d_antall_ulik_0),
    expected = svar_antall_ulik_0_95,
    tolerance = 1e-15
  ) # Standard skal være 95 %-KI
  expect_equal(aggreger_ki_rate(d_antall_ulik_0, konf_niva = 0.95),
    expected = svar_antall_ulik_0_95,
    tolerance = 1e-15
  )
  expect_equal(aggreger_ki_rate(d_antall_ulik_0, konf_niva = 0.9),
    expected = svar_antall_ulik_0_90,
    tolerance = 1e-15
  )
})

test_that("Funksjonen støtter angivelse av multiplikator", {
  d = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = rep(TRUE, 3)
  )
  svar_multiplikator_1000 = tibble(
    est = 47.297297297297298257,
    konfint_nedre = 26.644437703193837308,
    konfint_ovre = 76.570304030960272712
  )

  expect_equal(aggreger_ki_rate(d, multiplikator = 1000),
    expected = svar_multiplikator_1000,
    tolerance = testthat_tolerance()
  )
})


# Utdata på riktig format -------------------------------------------------

test_that("Funksjonen gjev alltid ut ugrupperte data", {
  d_test = tibble(
    sjukehus = factor(c("A", "B", "B")),
    post = factor(c("1", "1", "2")),
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  expect_length(group_vars(aggreger_ki_rate(group_by(d_test, sjukehus))), 0)
  expect_length(group_vars(aggreger_ki_rate(group_by(d_test, sjukehus, post))), 0)
})

test_that("Funksjonen gjev ut tibble når inndata er tibble, og data.frame når inndata er data.frame", {
  d_test_tibble = tibble(
    ki_antall = 5,
    ki_eksponering = 100,
    ki_aktuell = FALSE
  )
  svar_test_tibble = tibble(
    est = NA_real_,
    konfint_nedre = NA_real_,
    konfint_ovre = NA_real_
  )

  d_test_df = as.data.frame(d_test_tibble)
  svar_test_df = as.data.frame(svar_test_tibble)

  expect_identical(aggreger_ki_rate(d_test_tibble), svar_test_tibble)
  expect_identical(aggreger_ki_rate(d_test_df), svar_test_df)
})


test_that("Bruk av alfa gjev same svar som tilsvarande konf_niva", {
  d = tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, TRUE)
  )
  expect_identical(suppressWarnings(aggreger_ki_rate(d, alfa = 0.2)),
    expected = aggreger_ki_rate(d, konf_niva = 0.8)
  )
})
