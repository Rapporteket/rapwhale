# Aggregering - Beregn andel ----------------------------------------------

test_that("Feilmelding hvis ikke tibble/data.frame med nødvendige kolonner", {
  d_uten_nevner = tibble::tibble(foo = 1:3, ki_krit_teller = rep(TRUE, 3))
  d_uten_teller = tibble::tibble(foo = 1:3, ki_krit_nevner = rep(TRUE, 3))
  d_uten_begge = tibble::tibble(foo = 1:3)
  liste = list(ki_krit_teller = c(FALSE, TRUE, TRUE), ki_krit_nevner = c(TRUE, TRUE, TRUE))

  feilmelding_kol = "Inndata må være tibble/data.frame med kolonnene «ki_krit_teller» og «ki_krit_nevner»"
  expect_error(aggreger_ki_prop(liste), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_nevner), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_teller), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_begge), feilmelding_kol)
})

# test for feil variabeltyper
test_that("Feilmelding hvis data av feil type", {
  d_feil_teller_tekst = tibble::tibble(ki_krit_teller = c("0", "1", "1"), ki_krit_nevner = c(FALSE, TRUE, TRUE))
  d_feil_nevner_tekst = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, TRUE), ki_krit_nevner = c("0", "1", "1"))
  d_feil_teller_fak = tibble::tibble(ki_krit_teller = factor(c("5", "5", "5")), ki_krit_nevner = c(FALSE, TRUE, TRUE))
  d_feil_nevner_fak = tibble::tibble(ki_krit_teller = c(FALSE, FALSE, TRUE), ki_krit_nevner = c(factor(c("5", "5", "5"))))
  d_feil_teller_num = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, 2), ki_krit_nevner = c(TRUE, TRUE, TRUE))
  d_feil_nevner_num = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, TRUE), ki_krit_nevner = c(TRUE, TRUE, 2))

  feilmelding = "Kriterievariablene må være boolsk"
  expect_error(aggreger_ki_prop(d_feil_teller_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_teller_fak), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_teller_num), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_num), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_fak), feilmelding)
})

test_that("Feilmelding hvis kriterievariablene inneholder annet enn TRUE, FALSE og (for teller) NA eller er inkonsistente", {
  d_teller_med_feil_1 = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, TRUE), ki_krit_nevner = c(TRUE, TRUE, FALSE))
  d_teller_feil_og_na = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, NA), ki_krit_nevner = c(TRUE, TRUE, TRUE))

  feilmelding_teller = "«ki_krit_teller» må være TRUE eller FALSE hvis «ki_krit_nevner» er TRUE, og FALSE eller NA hvis «ki_krit_nevner» er FALSE"
  expect_error(aggreger_ki_prop(d_teller_med_feil_1), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_feil_og_na), feilmelding_teller)


  d_nevner_med_feil = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, TRUE), ki_krit_nevner = c(TRUE, TRUE, NA))

  feilmelding_nevner = "«ki_krit_nevner» må være TRUE eller FALSE"
  expect_error(aggreger_ki_prop(d_nevner_med_feil), feilmelding_nevner)
})

test_that("aggreger_ki_prop() fungerer (utan feilmelding) viss «ki_krit_nevner» er FALSE (og elles er gyldig)", {
  d_teller_ok_men_na = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, NA), ki_krit_nevner = c(TRUE, TRUE, FALSE))
  d_teller_ok_men_false = tibble::tibble(ki_krit_teller = c(FALSE, TRUE, FALSE), ki_krit_nevner = c(TRUE, TRUE, FALSE))
  d_teller_ok_men_na_res = tibble::tibble(est = 0.5, ki_teller = 1L, ki_nevner = 2L, konfint_nedre = 0.094531205734230739, konfint_ovre = 0.90546879426576921)

  expect_error(aggreger_ki_prop(d_teller_ok_men_na), NA)
  expect_error(aggreger_ki_prop(d_teller_ok_men_false), NA)
  expect_identical(aggreger_ki_prop(d_teller_ok_men_na), d_teller_ok_men_na_res)
  expect_identical(aggreger_ki_prop(d_teller_ok_men_false), d_teller_ok_men_na_res)
})

# Funksjonen må tillate tilfeller hvor sum teller_krit er 0.
test_that("Funksjonen tillater tilfeller hvor ingen observasjoner oppfyller kriteriet for teller", {
  d_ugruppert = tibble::tibble(ki_krit_teller = c(rep(FALSE, 3)), ki_krit_nevner = c(rep(FALSE, 3)))
  d_gruppert = tibble::tibble(
    sykehus = factor(rep(c("B", "A"), each = 3)),
    ki_krit_teller = c(rep(FALSE, 6)),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3))
  ) %>%
    dplyr::group_by(sykehus)

  svar_ugruppert = tibble::tibble(
    est = NA_real_,
    ki_teller = 0L, ki_nevner = 0L,
    konfint_nedre = NA_real_, konfint_ovre = NA_real_
  )

  svar_gruppert = tibble::tibble(
    sykehus = factor(c("A", "B")), est = c(NA_real_, 0),
    ki_teller = c(0L, 0L), ki_nevner = c(0L, 3L),
    konfint_nedre = c(NA_real_, 0), konfint_ovre = c(NA_real_, 0.5614970317550454)
  )

  expect_identical(aggreger_ki_prop(d_ugruppert), svar_ugruppert)
  expect_identical(aggreger_ki_prop(d_gruppert), svar_gruppert)
})

# 1) Hvordan skal funksjonen håndtere missing i grupperingsvariabel?

test_that("Funksjonen returnerer «NA» for de grupperte verdiene som ikke har noen øvrig gruppetilhørighet", {
  d_gruppert_med_na = suppressWarnings({
    tibble::tibble(
      sykehus = factor(rep(c("B", "A", NA), each = 3)),
      ki_krit_teller = c(rep(FALSE, 7), TRUE, FALSE),
      ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3), FALSE, TRUE, FALSE)
    ) %>%
      dplyr::group_by(sykehus)
  })

  svar_gruppert_med_na = tibble::tibble(
    sykehus = factor(c("A", "B", NA)),
    est = c(NA_real_, 0, 1),
    ki_teller = c(0L, 0L, 1L), ki_nevner = c(0L, 3L, 1L),
    konfint_nedre = c(NA_real_, 0, 0.2065493143772375), konfint_ovre = c(NA_real_, 0.5614970317550454, 1)
  )

  expect_identical(aggreger_ki_prop(d_gruppert_med_na), svar_gruppert_med_na)
})

# 2) Hvordan håndtere at grupperingsvariabel er en faktor som har nivå som ikke eksisterer i datasettet?
# Eks.:
test_that("Funksjonen gir en advarsel når det finnes ubrukte nivå i grupperingsvariabel (men likevel en rad for hvert *mulige* nivå)", {
  d_gruppert_ekstra_levels = tibble::tibble(
    sykehus = factor(rep(c("B", "A"), each = 3), levels = LETTERS[1:4]),
    ki_krit_teller = c(rep(FALSE, 6)),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3))
  ) %>%
    dplyr::group_by(sykehus, .drop = FALSE)

  d_svar_gruppert_ekstra_levels = tibble::tibble(
    sykehus = factor(LETTERS[1:4], levels = LETTERS[1:4]),
    est = c(NA, 0, NA, NA),
    ki_teller = rep(0L, 4),
    ki_nevner = c(0L, 3L, 0L, 0L),
    konfint_nedre = c(NA, 0, NA, NA),
    konfint_ovre = c(NA, 0.561497031755045, NA, NA)
  )

  feilmelding_ekstra_levels = "Det finnes grupper uten observasjoner i grupperingsvariabel"
  expect_warning(
    aggreger_ki_prop(d_gruppert_ekstra_levels),
    feilmelding_ekstra_levels
  )
  expect_equal(
    suppressWarnings(aggreger_ki_prop(d_gruppert_ekstra_levels)),
    d_svar_gruppert_ekstra_levels
  )
})

# Hvordan skal funksjonen håndtere tilfeller med grupper der alle
# ki_nevner-verdiene er FALSE?
# Vil her at vi får ut en oppføring for *HVER* gruppe, inkludert de som ikke har noen observasjoner
# Grupper som ikke har noen verdier kan eventuelt filtreres ut i etterkant hvis de ikke skal være med i figurer, etc.
test_that("Funksjonen returnerer verdier for alle grupper i inndata, selv de gruppene som ikke inneholder observasjoner", {
  d_grupper_uten_innhold = tibble::tibble(
    sykehus = factor(rep(c("B", "A", "C"), each = 3)),
    ki_krit_teller = c(rep(FALSE, 6), TRUE, TRUE, FALSE),
    ki_krit_nevner = c(rep(c(TRUE, FALSE), each = 3), TRUE, TRUE, FALSE)
  ) %>%
    group_by(sykehus)

  svar_uten_innhold = tibble::tibble(
    sykehus = factor(c("A", "B", "C")),
    est = c(NA_real_, 0, 1),
    ki_teller = c(0L, 0L, 2L), ki_nevner = c(0L, 3L, 2L),
    konfint_nedre = c(NA_real_, 0, 0.3423802275066532), konfint_ovre = c(NA_real_, 0.5614970317550454, 1)
  )

  expect_identical(aggreger_ki_prop(d_grupper_uten_innhold), svar_uten_innhold)
})


test_that("Funksjonen gir forventet resultat", {
  d_25 = tibble::tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(rep(TRUE, 4))
  )
  svar_25 = tibble::tibble(est = 0.25, ki_teller = 1L, ki_nevner = 4L, konfint_nedre = 0.04558726080970059, konfint_ovre = 0.699358157417598)

  d_33 = tibble::tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  svar_33 = tibble::tibble(est = 0.3333333333333333, ki_teller = 1L, ki_nevner = 3L, konfint_nedre = 0.06149194472039624, konfint_ovre = 0.7923403991979524)

  d_50 = tibble::tibble(
    ki_krit_teller = c(rep(c(TRUE, FALSE), each = 2)),
    ki_krit_nevner = c(rep(TRUE, 4))
  )
  svar_50 = tibble::tibble(est = 0.5, ki_teller = 2L, ki_nevner = 4L, konfint_nedre = 0.15003898915214955, konfint_ovre = 0.84996101084785047)

  d_67 = tibble::tibble(
    ki_krit_teller = c(TRUE, TRUE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  svar_67 = tibble::tibble(est = 0.6666666666666666, ki_teller = 2L, ki_nevner = 3L, konfint_nedre = 0.20765960080204776, konfint_ovre = 0.93850805527960379)

  d_75 = tibble::tibble(
    ki_krit_teller = c(TRUE, TRUE, TRUE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, TRUE)
  )
  svar_75 = tibble::tibble(est = 0.75, ki_teller = 3L, ki_nevner = 4L, konfint_nedre = 0.300641842582402, konfint_ovre = 0.9544127391902995)

  expect_identical(aggreger_ki_prop(d_25), svar_25)
  expect_identical(aggreger_ki_prop(d_33), svar_33)
  expect_identical(aggreger_ki_prop(d_50), svar_50)
  expect_identical(aggreger_ki_prop(d_67), svar_67)
  expect_identical(aggreger_ki_prop(d_75), svar_75)
})

test_that("Funksjonen støtter angivelse av konfidensinvå", {
  d_test = tibble::tibble(
    ki_krit_teller = c(TRUE, FALSE, FALSE, FALSE),
    ki_krit_nevner = c(TRUE, TRUE, TRUE, FALSE)
  )
  d_svar_05 = tibble::tibble(
    est = 0.3333333333333333, ki_teller = 1L, ki_nevner = 3L,
    konfint_nedre = 0.06149194472039624, konfint_ovre = 0.7923403991979524
  )
  d_svar_10 = tibble::tibble(
    est = 0.3333333333333333, ki_teller = 1L, ki_nevner = 3L,
    konfint_nedre = 0.07826572633372843, konfint_ovre = 0.7464661317187757
  )

  expect_identical(aggreger_ki_prop(d_test), d_svar_05) # Standard skal være 95 %-KI
  expect_identical(aggreger_ki_prop(d_test, alfa = .05), d_svar_05)
  expect_identical(aggreger_ki_prop(d_test, alfa = .10), d_svar_10)
})
