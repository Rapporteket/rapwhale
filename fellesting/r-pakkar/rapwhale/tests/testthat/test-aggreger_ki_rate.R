# Aggregering - Beregn rater ----------------------------------------------


# Feilmeldinger -----------------------------------------------------------

test_that("Feilmelding hvis ikke tibble/data.frame med nødvendige kolonner", {
  d = tibble::tibble(
    ki_antall = 1,
    ki_eksponering = 1,
    ki_aktuell = TRUE
  )

  feilmelding_kol = paste0(
    "Inndata må være tibble/data.frame med kolonnene «ki_antall», ",
    "«ki_eksponering» og «ki_aktuell»"
  )

  expect_error(aggreger_ki_rate(dplyr::select(d, -ki_antall)), feilmelding_kol)
  expect_error(aggreger_ki_rate(dplyr::select(d, -ki_eksponering)), feilmelding_kol)
  expect_error(aggreger_ki_rate(dplyr::select(d, -ki_aktuell)), feilmelding_kol)
  expect_error(aggreger_ki_rate(unclass(d)), feilmelding_kol)
})

test_that("Feilmelding hvis data av feil type", {
  d_feil_antall_tekst = tibble::tibble(
    ki_antall = c("5", "2", "7"),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_tekst = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c("100", "95", "101"),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_tekst = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c("TRUE", "TRUE", "FALSE")
  )
  d_feil_antall_fak = tibble::tibble(
    ki_antall = factor(c(5, 2, 7)),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_fak = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = factor(c(100, 95, 101)),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_fak = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = factor(c(TRUE, TRUE, FALSE))
  )
  d_feil_antall_log = tibble::tibble(
    ki_antall = c(TRUE, TRUE, FALSE),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_eksponering_log = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(TRUE, TRUE, FALSE),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_feil_aktuell_num = tibble::tibble(
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
  d_aktuell_ikke_ok = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, NA, FALSE)
  )

  feilmelding = "«ki_aktuell» må være TRUE eller FALSE"

  expect_error(aggreger_ki_rate(d_aktuell_ikke_ok), feilmelding)
})

test_that("Feilmelding hvis ki_antall eller ki_eksponering er missing dersom ki_aktuell er TRUE", {
  d_antall_ikke_ok = tibble::tibble(
    ki_antall = c(5, NA_real_, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_eksponering_ikke_ok = tibble::tibble(
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
  d_antall_lavere_0 = tibble::tibble(
    ki_antall = c(5, -1, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding = "«ki_antall» kan ikke være mindre enn 0"

  expect_error(aggreger_ki_rate(d_antall_lavere_0), feilmelding)
})

test_that("Feilmelding hvis minst en ki_eksponering er mindre enn eller lik 0", {
  d_eksponering_lavere_0 = tibble::tibble(
    ki_antall = c(5, NA_real_, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  d_eksponering_lik_0 = tibble::tibble(
    ki_antall = c(5, NA_real_, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )

  feilmelding = "«ki_eksponering» kan ikke være mindre enn eller lik 0"

  expect_error(aggreger_ki_rate(d_eksponering_lavere_0), feilmelding)
  expect_error(aggreger_ki_rate(d_eksponering_lik_0), feilmelding)
})

test_that("Feilmelding hvis alfa ikke er et tall mellom 0 og 1", {
  d_var_ok = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuel = c(TRUE, TRUE, FALSE)
  )

  feilmelding_alfa = "«alfa» må være et tall mellom 0 og 1"

  expect_error(aggreger_ki_rate(d_var_ok, alfa = 1.2), feilmelding_alfa)
  expect_error(aggreger_ki_rate(d_var_ok, alfa = 0), feilmelding_alfa)
  expect_error(aggreger_ki_rate(d_var_ok, alfa = 1), feilmelding_alfa)
  expect_error(aggreger_ki_rate(d_var_ok, alfa = "0.05"), feilmelding_alfa)
})

test_that("Feilmelding hvis multiplikator ikke er et positivt heltall", {
  d_var_ok = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuel = c(TRUE, TRUE, FALSE)
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
    aggreger_ki_rate(d_var_ok, multiplikator = NA_integer_),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = "1000"),
    feilmelding_multiplikator
  )
})

test_that("Feilmelding hvis alfa eller multiplikator ikke har lengde 1", {
  d_var_ok = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuel = c(TRUE, TRUE, FALSE)
  )

  feilmelding_alfa = "«alfa» må ha lengde 1"
  feilmelding_multiplikator = "«multiplikator» må ha lengde 1"

  expect_error(
    aggreger_ki_rate(d_var_ok, alfa = c()),
    feilmelding_alfa
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, alfa = c(0.05, 0.1)),
    feilmelding_alfa
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = c()),
    feilmelding_multiplikator
  )
  expect_error(
    aggreger_ki_rate(d_var_ok, multiplikator = c(1000, 100)),
    feilmelding_multiplikator
  )
})


# Tester med NA og antall = 0 ---------------------------------------------

test_that("aggreger_ki_rate() fungerer (uten feilmelding) om ki_aktuell er FALSE (og ellers er gyldig)", {
  d_antall_ok_men_na = tibble::tibble(
    ki_antall = c(5, 2, NA),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_ok_man_na = tibble::tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  d_eksponering_ok_men_na = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, NA),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_eksponering_ok_man_na = tibble::tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  expect_identical(
    aggreger_ki_rate(d_antall_ok_men_na),
    svar_antall_ok_man_na
  )
  expect_identical(
    aggreger_ki_rate(d_eksponering_ok_men_na),
    svar_eksponering_ok_man_na
  )
})

test_that("Funksjonen tillater tilfeller hvor antall observasjoner er 0", {
  d_ugruppert = tibble::tibble(
    ki_antall = rep(0, 3),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_ugruppert = tibble::tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.015362729607969183993
  )

  d_gruppert = tibble::tibble(
    sykehus = factor(rep(c("B", "A"), each = 3)),
    ki_antall = rep(0, 6),
    ki_eksponering = c(100, 95, 101, 200, 190, 202),
    ki_aktuell = c(TRUE, TRUE, FALSE, rep(FALSE, 3))
  ) %>%
    dplyr::group_by(sykehus)
  svar_gruppert = tibble::tibble(
    est = c(NA_real_, 0),
    konfint_nedre = c(NA_real_, 0),
    konfint_ovre = c(NA_real_, 0.015362729607969183993)
  )

  expect_identical(aggreger_ki_rate(d_ugruppert), svar_ugruppert)
  expect_identical(aggreger_ki_rate(d_gruppert), svar_gruppert)
})


# Grupperingstester -------------------------------------------------------

test_that("Funksjonen returnerer «NA» for de grupperte verdiene som ikke har noen øvrig gruppetilhørighet", {
  d_gruppert_med_na = suppressWarnings({
    tibble::tibble(
      sykehus = factor(rep(c("B", "A", NA), each = 3)),
      ki_antall = c(rep(0, 6), 5, 2, 7),
      ki_eksponering = c(100, 95, 101, 200, 190, 202, 50, 47, 51),
      ki_aktuell = c(rep(c(TRUE, FALSE), each = 3), FALSE, TRUE, FALSE)
    ) %>%
      dplyr::group_by(sykehus)
  })
  svar_gruppert_med_na = tibble::tibble(
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

  expect_identical(aggreger_ki_rate(d_gruppert_med_na), svar_gruppert_med_na)
})

test_that("Funksjonen gir en advarsel når det finnes ubrukte nivå i grupperingsvariabel (men likevel en rad for hvert *mulige* nivå)", {
  d_gruppert_ekstra_levels = tibble::tibble(
    sykehus = factor(rep(c("B", "A"), each = 3), levels = LETTERS[1:4]),
    ki_antall = c(rep(0, 6)),
    ki_eksponering = c(100, 95, 101, 200, 190, 202),
    ki_aktuell = c(rep(c(TRUE, FALSE), each = 3))
  ) %>%
    dplyr::group_by(sykehus, .drop = FALSE)
  svar_gruppert_ekstra_levels = tibble::tibble(
    sykehus = factor(LETTERS[1:4], levels = LETTERS[1:4]),
    est = c(NA_real_, 0, NA_real_, NA_real_),
    konfint_nedre = c(NA_real_, 0, NA_real_, NA_real_),
    konfint_ovre = c(
      NA_real_,
      0.010120717140385104144,
      NA_real_,
      NA_real_
    )
  )

  feilmelding_ekstra_levels = "Det finnes grupper uten observasjoner i grupperingsvariabel"

  expect_warning(
    aggreger_ki_rate(d_gruppert_ekstra_levels),
    feilmelding_ekstra_levels
  )
  expect_equal(
    suppressWarnings(aggreger_ki_rate(d_gruppert_ekstra_levels)),
    svar_gruppert_ekstra_levels
  )
})

test_that("Funksjonen returnerer en tom ugruppert tibble med riktige kolonner hvis inndata er gruppert med null rader", {
  d_gruppert_tom = tibble::tibble(
    sykehus = factor(),
    ki_antall = numeric(),
    ki_eksponering = numeric(),
    ki_krit_nevner = logical()
  ) %>%
    group_by(sykehus)
  svar_tom = tibble::tibble(
    sykehus = factor(),
    est = numeric(),
    konfint_nedre = numeric(),
    konfint_ovre = numeric()
  )

  expect_identical(aggreger_ki_prop(d_gruppert_tom), svar_tom)
})


# Rigktig resultat --------------------------------------------------------

test_that("Funksjonen gir ut forventet resultat", {
  d_ingen = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = rep(FALSE, 3)
  )
  svar_ingen = tibble::tibble(
    est = NA_real_,
    konfint_nedre = NA_real_,
    konfint_ovre = NA_real_
  )

  d_alle = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = rep(TRUE, 3)
  )
  svar_alle = tibble::tibble(
    est = 0.047297297297297299923,
    konfint_nedre = 0.026644437703193836475,
    konfint_ovre = 0.076570304030960267827
  )
  svar_alle_multiplikator_1000 = svar_alle * 1000

  d_noen = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_noen = tibble::tibble(
    est = 0.035897435897435894803,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )

  expect_identical(aggreger_ki_rate(d_ingen), svar_ingen)
  expect_identical(aggreger_ki_rate(d_alle), svar_alle)
  expect_identical(
    aggreger_ki_rate(d_alle, multiplikator = 1000),
    svar_alle_multiplikator_1000
  )
  expect_identical(aggreger_ki_rate(d_noen), svar_noen)
})

test_that("Funksjonen støtter angivelse av konfidensinvå", {
  d_antall_lik_0 = tibble::tibble(
    ki_antall = rep(0, 3),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_lik_0_05 = tibble::tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.015362729607969183993
  )
  svar_antall_lik_0_10 = tibble::tibble(
    est = 0,
    konfint_nedre = 0,
    konfint_ovre = 0.011808128682020746503
  )

  d_antall_ulik_0 = tibble::tibble(
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  svar_antall_ulik_0_05 = tibble::tibble(
    est = 7 / 195,
    konfint_nedre = 0.015422856627814649638,
    konfint_ovre = 0.069418816705480224094
  )
  svar_antall_ulik_0_10 = tibble::tibble(
    est = 7 / 195,
    konfint_nedre = 0.017943117460240590177,
    konfint_ovre = 0.063051223283699317501
  )

  expect_identical(
    aggreger_ki_prop(d_antall_lik_0),
    svar_antall_lik_0_05
  ) # Standard skal være 95 %-KI
  expect_identical(
    aggreger_ki_prop(d_antall_lik_0, alfa = .05),
    svar_antall_lik_0_05
  )
  expect_identical(
    aggreger_ki_prop(d_antall_lik_0, alfa = .10),
    svar_antall_lik_0_10
  )
  expect_identical(
    aggreger_ki_prop(d_antall_ulik_0),
    svar_antall_ulik_0_05
  ) # Standard skal være 95 %-KI
  expect_identical(
    aggreger_ki_prop(d_antall_ulik_0, alfa = .05),
    svar_antall_ulik_0_05
  )
  expect_identical(
    aggreger_ki_prop(d_antall_ulik_0, alfa = .10),
    svar_antall_ulik_0_10
  )
})


# Utdata på riktig format -------------------------------------------------

test_that("Funksjonen gjev alltid ut ugrupperte data", {
  d_test = tibble::tibble(
    sjukehus = factor(c("A", "B", "B")),
    post = factor(c("1", "1", "2")),
    ki_antall = c(5, 2, 7),
    ki_eksponering = c(100, 95, 101),
    ki_aktuell = c(TRUE, TRUE, FALSE)
  )
  expect_length(group_vars(aggreger_ki_prop(group_by(d_test, sjukehus))), 0)
  expect_length(group_vars(aggreger_ki_prop(group_by(d_test, sjukehus, post))), 0)
})

test_that("Funksjonen gjev ut tibble når inndata er tibble, og data.frame når inndata er data.frame", {
  d_test_tibble = tibble::tibble(
    ki_antall = 5,
    ki_eksponering = 100,
    ki_aktuell = FALSE
  )
  svar_test_tibble = tibble::tibble(
    est = NA_real_,
    konfint_nedre = NA_real_,
    konfint_ovre = NA_real_
  )

  d_test_df = as.data.frame(d_test_tibble)
  svar_test_df = as.data.frame(d_res_tibble)

  expect_identical(aggreger_ki_prop(d_test_tibble), svar_test_tibble)
  expect_identical(aggreger_ki_prop(d_test_df), svar_test_df)
})
