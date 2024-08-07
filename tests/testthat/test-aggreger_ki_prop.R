# Aggregering - Beregn andel ----------------------------------------------

test_that("Feilmelding hvis ikke tibble/data.frame med nødvendige kolonner", {
  d_uten_nevner = tibble(foo = 1:3, ki_krit_teller = rep(TRUE, 3))
  d_uten_teller = tibble(foo = 1:3, ki_krit_nevner = rep(TRUE, 3))
  d_uten_begge = tibble(foo = 1:3)
  liste = list(
    ki_krit_teller = c(FALSE, TRUE, TRUE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE)
  )

  feilmelding_kol = paste0(
    "Inndata må være tibble/data.frame med kolonnene ",
    "«ki_krit_teller» og «ki_krit_nevner»"
  )
  expect_error(aggreger_ki_prop(liste), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_nevner), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_teller), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_begge), feilmelding_kol)
})

# test for feil variabeltyper
test_that("Feilmelding hvis data av feil type", {
  d_feil_teller_tekst = tibble(
    ki_krit_teller = c("0", "1", "1"),
    ki_krit_nevner = c(FALSE, TRUE, TRUE)
  )
  d_feil_nevner_tekst = tibble(
    ki_krit_teller = c(FALSE, TRUE, TRUE),
    ki_krit_nevner = c("0", "1", "1")
  )
  d_feil_teller_fak = tibble(
    ki_krit_teller = factor(c("5", "5", "5")),
    ki_krit_nevner = c(FALSE, TRUE, TRUE)
  )
  d_feil_nevner_fak = tibble(
    ki_krit_teller = c(FALSE, FALSE, TRUE),
    ki_krit_nevner = c(factor(c("5", "5", "5")))
  )
  d_feil_teller_num = tibble(
    ki_krit_teller = c(FALSE, TRUE, 2),
    ki_krit_nevner = c(TRUE, TRUE, TRUE)
  )
  d_feil_nevner_num = tibble(
    ki_krit_teller = c(FALSE, TRUE, TRUE),
    ki_krit_nevner = c(TRUE, TRUE, 2)
  )

  feilmelding = "Kriterievariablene må være logiske variabler"
  expect_error(aggreger_ki_prop(d_feil_teller_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_teller_fak), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_teller_num), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_num), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_fak), feilmelding)
})

test_that(paste0(
  "Feilmelding hvis kriterievariablene inneholder annet enn TRUE, ",
  "FALSE og (for teller) NA eller er inkonsistente"
), {
  d_teller_med_feil_1 = tibble(
    ki_krit_teller = c(FALSE, TRUE, TRUE),
    ki_krit_nevner = c(TRUE, TRUE, FALSE)
  )
  d_teller_feil_og_na = tibble(
    ki_krit_teller = c(FALSE, TRUE, NA),
    ki_krit_nevner = c(TRUE, TRUE, TRUE)
  )

  feilmelding_teller = paste0(
    "«ki_krit_teller» må være TRUE eller FALSE hvis «ki_krit_nevner» er TRUE, ",
    "og FALSE eller NA hvis «ki_krit_nevner» er FALSE"
  )
  expect_error(aggreger_ki_prop(d_teller_med_feil_1), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_feil_og_na), feilmelding_teller)


  d_nevner_med_feil = tibble(
    ki_krit_teller = c(FALSE, TRUE, TRUE),
    ki_krit_nevner = c(TRUE, TRUE, NA)
  )

  feilmelding_nevner = "«ki_krit_nevner» må være TRUE eller FALSE"
  expect_error(aggreger_ki_prop(d_nevner_med_feil), feilmelding_nevner)
})

test_that("Feilmelding hvis konf_niva ikke er et tall mellom 0 og 1", {
  d_teller_ok = tibble(
    ki_krit_teller = c(FALSE, TRUE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, FALSE)
  )
  feilmelding_konf_niva = "«konf_niva» må være et tall mellom 0 og 1"
  expect_error(aggreger_ki_prop(d_teller_ok, konf_niva = 1.2),
    regexp = feilmelding_konf_niva
  )
  expect_error(aggreger_ki_prop(d_teller_ok, konf_niva = 0),
    regexp = feilmelding_konf_niva
  )
  expect_error(aggreger_ki_prop(d_teller_ok, konf_niva = 1),
    regexp = feilmelding_konf_niva
  )
  expect_error(aggreger_ki_prop(d_teller_ok, konf_niva = "0.05"),
    regexp = feilmelding_konf_niva
  )
})

test_that(paste0(
  "aggreger_ki_prop() fungerer (utan feilmelding) ",
  "viss «ki_krit_nevner» er FALSE (og elles er gyldig)"
), {
  d_teller_ok_men_na = tibble(
    ki_krit_teller = c(FALSE, TRUE, NA),
    ki_krit_nevner = c(TRUE, TRUE, FALSE)
  )
  d_teller_ok_men_false = tibble(
    ki_krit_teller = c(FALSE, TRUE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, FALSE)
  )
  d_teller_ok_men_na_res = tibble(
    est = 0.5,
    ki_teller = 1L,
    ki_nevner = 2L,
    konfint_nedre = 0.094531205734230739,
    konfint_ovre = 0.90546879426576921
  )

  expect_no_error(aggreger_ki_prop(d_teller_ok_men_na))
  expect_no_error(aggreger_ki_prop(d_teller_ok_men_false))
  expect_equal(aggreger_ki_prop(d_teller_ok_men_na),
    expected = d_teller_ok_men_na_res,
    tolerance = testthat_tolerance()
  )
  expect_equal(aggreger_ki_prop(d_teller_ok_men_false),
    expected = d_teller_ok_men_na_res,
    tolerance = testthat_tolerance()
  )
})

# Funksjonen må tillate tilfeller hvor sum teller_krit er 0.
test_that(paste0(
  "Funksjonen tillater tilfeller hvor ingen observasjoner ",
  "oppfyller kriteriet for teller"
), {
  d_ugruppert = tibble(
    ki_krit_teller = c(rep(FALSE, 3)),
    ki_krit_nevner = c(rep(FALSE, 3))
  )
  d_gruppert = tibble(
    sykehus = factor(rep(c("B", "A"), each = 3)),
    ki_krit_teller = c(rep(FALSE, 6)),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3))
  ) |>
    group_by(sykehus)

  svar_ugruppert = tibble(
    est = NA_real_,
    ki_teller = 0L, ki_nevner = 0L,
    konfint_nedre = NA_real_, konfint_ovre = NA_real_
  )

  svar_gruppert = tibble(
    sykehus = factor(c("A", "B")),
    est = c(NA_real_, 0),
    ki_teller = c(0L, 0L),
    ki_nevner = c(0L, 3L),
    konfint_nedre = c(NA_real_, 0),
    konfint_ovre = c(NA_real_, 0.5614970317550454)
  )

  expect_identical(aggreger_ki_prop(d_ugruppert), svar_ugruppert)
  expect_identical(aggreger_ki_prop(d_gruppert), svar_gruppert)
})

# 1) Hvordan skal funksjonen håndtere missing i grupperingsvariabel?

test_that(paste0(
  "Funksjonen returnerer «NA» for de grupperte verdiene ",
  "som ikke har noen øvrig gruppetilhørighet"
), {
  d_gruppert_med_na = suppressWarnings({
    tibble(
      sykehus = factor(rep(c("B", "A", NA), each = 3)),
      ki_krit_teller = c(rep(FALSE, 7), TRUE, FALSE),
      ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3), FALSE, TRUE, FALSE)
    ) |>
      group_by(sykehus)
  })

  svar_gruppert_med_na = tibble(
    sykehus = factor(c("A", "B", NA)),
    est = c(NA_real_, 0, 1),
    ki_teller = c(0L, 0L, 1L),
    ki_nevner = c(0L, 3L, 1L),
    konfint_nedre = c(NA_real_, 0, 0.2065493143772375),
    konfint_ovre = c(NA_real_, 0.5614970317550454, 1)
  )

  expect_identical(aggreger_ki_prop(d_gruppert_med_na), svar_gruppert_med_na)
})

# 2) Hvordan håndtere at grupperingsvariabel er en faktor som har nivå som ikke eksisterer i datasettet?
# Eks.:
test_that(paste0(
  "Funksjonen gir en advarsel når det finnes ubrukte nivå i ",
  "grupperingsvariabel (men likevel en rad for hvert *mulige* nivå)"
), {
  d_gruppert_ekstra_levels = tibble(
    sykehus = factor(rep(c("B", "A"), each = 3), levels = LETTERS[1:4]),
    ki_krit_teller = c(rep(FALSE, 6)),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3))
  ) |>
    group_by(sykehus, .drop = FALSE)

  d_svar_gruppert_ekstra_levels = tibble(
    sykehus = factor(LETTERS[1:4], levels = LETTERS[1:4]),
    est = c(NA, 0, NA, NA),
    ki_teller = rep(0L, 4),
    ki_nevner = c(0L, 3L, 0L, 0L),
    konfint_nedre = c(NA, 0, NA, NA),
    konfint_ovre = c(NA, 0.561497031755045394874, NA, NA)
  )

  feilmelding_ekstra_levels = paste0(
    "Det finnes grupper uten observasjoner i grupperingsvariabel"
  )
  expect_warning(
    aggreger_ki_prop(d_gruppert_ekstra_levels),
    feilmelding_ekstra_levels
  )
  expect_equal(
    object = suppressWarnings(aggreger_ki_prop(d_gruppert_ekstra_levels)),
    expected = d_svar_gruppert_ekstra_levels,
    tolerance = testthat_tolerance()
  )
})

test_that(paste0(
  "Funksjonen returnerer verdier for alle grupper i inndata, ",
  "selv de gruppene som ikke inneholder observasjoner"
), {
  d_grupper_uten_innhold = tibble(
    sykehus = factor(rep(c("B", "A", "C"), each = 3)),
    ki_krit_teller = c(rep(FALSE, 6), TRUE, TRUE, FALSE),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3), TRUE, TRUE, FALSE)
  ) |>
    group_by(sykehus)

  svar_uten_innhold = tibble(
    sykehus = factor(c("A", "B", "C")),
    est = c(NA_real_, 0, 1),
    ki_teller = c(0L, 0L, 2L),
    ki_nevner = c(0L, 3L, 2L),
    konfint_nedre = c(NA_real_, 0, 0.3423802275066532),
    konfint_ovre = c(NA_real_, 0.5614970317550454, 1)
  )

  expect_identical(aggreger_ki_prop(d_grupper_uten_innhold), svar_uten_innhold)
})

test_that(paste0(
  "Funksjonen returnerer en tom ugruppert tibble med riktige kolonner ",
  "hvis inndata er gruppert med null rader"
), {
  d_gruppert_tom = tibble(
    sykehus = factor(),
    ki_krit_teller = logical(),
    ki_krit_nevner = logical()
  ) |>
    group_by(sykehus)

  svar_tom = tibble(
    sykehus = factor(),
    est = numeric(),
    ki_teller = integer(),
    ki_nevner = integer(),
    konfint_nedre = numeric(),
    konfint_ovre = numeric()
  )

  expect_identical(
    aggreger_ki_prop(d_gruppert_tom),
    svar_tom
  )
})

test_that("Funksjonen gir forventet resultat", {
  d_25 = tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(rep(TRUE, 4))
  )
  svar_25 = tibble(
    est = 0.25,
    ki_teller = 1L,
    ki_nevner = 4L,
    konfint_nedre = 0.04558726080970059,
    konfint_ovre = 0.699358157417598
  )

  d_33 = tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  svar_33 = tibble(
    est = 0.3333333333333333,
    ki_teller = 1L,
    ki_nevner = 3L,
    konfint_nedre = 0.06149194472039624,
    konfint_ovre = 0.7923403991979524
  )

  d_50 = tibble(
    ki_krit_teller = c(rep(c(TRUE, FALSE), each = 2)),
    ki_krit_nevner = c(rep(TRUE, 4))
  )
  svar_50 = tibble(
    est = 0.5,
    ki_teller = 2L,
    ki_nevner = 4L,
    konfint_nedre = 0.15003898915214955,
    konfint_ovre = 0.84996101084785047
  )

  d_67 = tibble(
    ki_krit_teller = c(TRUE, TRUE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  svar_67 = tibble(
    est = 0.6666666666666666,
    ki_teller = 2L,
    ki_nevner = 3L,
    konfint_nedre = 0.20765960080204776,
    konfint_ovre = 0.93850805527960379
  )

  d_75 = tibble(
    ki_krit_teller = c(TRUE, TRUE, TRUE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, TRUE)
  )
  svar_75 = tibble(
    est = 0.75,
    ki_teller = 3L,
    ki_nevner = 4L,
    konfint_nedre = 0.300641842582402,
    konfint_ovre = 0.9544127391902995
  )

  expect_equal(aggreger_ki_prop(d_25), svar_25, tolerance = testthat_tolerance())
  expect_equal(aggreger_ki_prop(d_33), svar_33, tolerance = testthat_tolerance())
  expect_equal(aggreger_ki_prop(d_50), svar_50, tolerance = testthat_tolerance())
  expect_equal(aggreger_ki_prop(d_67), svar_67, tolerance = testthat_tolerance())
  expect_equal(aggreger_ki_prop(d_75), svar_75, tolerance = testthat_tolerance())
})

test_that("Funksjonen støtter angivelse av konfidensinvå", {
  d_test = tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  d_svar_95 = tibble(
    est = 0.3333333333333333, ki_teller = 1L, ki_nevner = 3L,
    konfint_nedre = 0.06149194472039624, konfint_ovre = 0.7923403991979524
  )
  d_svar_90 = tibble(
    est = 0.3333333333333333, ki_teller = 1L, ki_nevner = 3L,
    konfint_nedre = 0.07826572633372843, konfint_ovre = 0.7464661317187757
  )

  expect_identical(aggreger_ki_prop(d_test), d_svar_95) # Standard skal være 95 %-KI
  expect_identical(aggreger_ki_prop(d_test, konf_niva = 0.95), d_svar_95)
  expect_identical(aggreger_ki_prop(d_test, konf_niva = 0.90), d_svar_90)
})

test_that("Funksjonen gjev alltid ut ugrupperte data", {
  d_test = tibble(
    sjukehus = factor(c("A", "B", "B")),
    post = factor(c("1", "1", "2")),
    ki_krit_teller = c(TRUE, FALSE, TRUE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE)
  )
  expect_length(group_vars(aggreger_ki_prop(group_by(d_test, sjukehus))), 0)
  expect_length(group_vars(aggreger_ki_prop(group_by(d_test, sjukehus, post))), 0)
})

test_that(paste0(
  "Funksjonen gjev ut tibble når inndata er tibble, ",
  "og data.frame når inndata er data.frame"
), {
  d_test_tibble = tibble(
    ki_krit_teller = FALSE,
    ki_krit_nevner = FALSE
  )
  d_test_df = as.data.frame(d_test_tibble)
  d_res_tibble = tibble(
    est = NA_real_, ki_teller = 0L, ki_nevner = 0L,
    konfint_nedre = NA_real_, konfint_ovre = NA_real_
  )
  d_res_df = as.data.frame(d_res_tibble)
  expect_identical(aggreger_ki_prop(d_test_tibble), d_res_tibble)
  expect_identical(aggreger_ki_prop(d_test_df), d_res_df)
})

test_that("Bruk av alfa gjev same svar som tilsvarande konf_niva", {
  d = tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(rep(TRUE, 4))
  )
  expect_identical(suppressWarnings(aggreger_ki_prop(d, alfa = 0.2)),
    expected = aggreger_ki_prop(d, konf_niva = 0.8)
  )
})
