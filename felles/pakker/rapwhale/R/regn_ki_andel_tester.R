library(testthat)

# Aggregeringsfunksjoner:

# Aggregering - Beregn andel ----------------------------------------------

context("Sjekker at inndata er på forventet format")

test_that("Feilmelding hvis nødvendige kolonner mangler", {
  d_uten_nevner = tibble(foo = 1:3, ki_krit_teller = rep(1, 3))
  d_uten_telle = tibble(foo = 1:3, ki_krit_nevner = rep(1, 3))
  d_uten_begge = tibble(foo = 1:3)
  feilmelding_kol = "Inndata må ha både 'ki_krit_teller' og 'ki_krit_nevner'"
  expect_error(aggreger_ki_prop(d_uten_nevner), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_teller), feilmelding_kol)
  expect_error(aggreger_ki_prop(d_uten_begge), feilmelding_kol)
})

# test for feil variabeltyper
test_that("Feilmelding hvis data av feil type", {
  d_feil_teller_tekst = tibble(ki_krit_teller = c("0", "1", "1"), ki_krit_nevner = c(0, 1, 1))
  d_feil_nevner_tekst = tibble(ki_krit_teller = c(0, 1, 1), ki_krit_nevner = c("0", "1", "1"))
  d_feil_teller_fak = tibble(ki_krit_teller = factor(c("5", "5", "5")), ki_krit_nevner = c(0, 1, 1))

  liste = list(ki_krit_teller = c(0, 1, 1), ki_krit_nevner = c(1, 1, 1))

  feilmelding = "Kriterievariablene må være tall"
  expect_error(aggreger_ki_prop(d_feil_teller_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_nevner_tekst), feilmelding)
  expect_error(aggreger_ki_prop(d_feil_teller_fak), feilmelding)

  expect_error(aggreger_ki_prop(liste), "Inndata må være data.frame eller tibble")
})

test_that("Feilmelding hvis kriterievariablene inneholder annet enn 0, 1 og (for teller) NA eller er inkonsistente", {
  d_teller_med_feil_1 = tibble(ki_krit_teller = c(0, 1, 2), ki_krit_nevner = c(1, 1, 1))
  d_teller_med_feil_2 = tibble(ki_krit_teller = c(0, 1, 2), ki_krit_nevner = c(1, 1, 0))
  d_teller_med_feil_3 = tibble(ki_krit_teller = c(0, 1, 1), ki_krit_nevner = c(1, 1, 0))
  d_teller_feil_og_na = tibble(ki_krit_teller = c(0, 1, NA), ki_krit_nevner = c(1, 1, 1))
  d_teller_ok_men_na = tibble(ki_krit_teller = c(0, 1, NA), ki_krit_nevner = c(1, 1, 0))

  d_nevner_med_feil_1 = tibble(ki_krit_teller = c(0, 1, 1), ki_krit_nevner = c(1, 1, 2))
  d_nevner_med_feil_2 = tibble(ki_krit_teller = c(0, 1, 1), ki_krit_nevner = c(1, 1, NA))

  feilmelding_teller = "'ki_krit_teller' må være 0 eller 1 (ev. NA hvis 'ki_krit_nevner' er 0)"
  expect_error(aggreger_ki_prop(d_teller_med_feil_1), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_med_feil_2), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_med_feil_3), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_feil_og_na), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_teller_ok_men_na), feilmelding_teller)

  feilmelding_nevner = "'ki_krit_nevner' må være 0 eller 1"
  expect_error(aggreger_ki_prop(d_nevner_med_feil_1), feilmelding_teller)
  expect_error(aggreger_ki_prop(d_nevner_med_feil_2), feilmelding_teller)
})


context("Sjekker grensetilfeller i inndata")

# Funksjonen må tillate tilfeller hvor sum teller_krit er 0.
test_that("Funksjonen tillater tilfeller hvor ingen observasjoner oppfyller kriteriet for teller", {
  d_ugruppert = tibble(ki_krit_teller = c(0, 0, 0), ki_krit_nevner = c(0, 0, 0))
  d_gruppert = tibble(
    sykehus = factor(rep(c("B", "A"), each = 3)),
    ki_krit_teller = c(0, 0, 0, 0, 0, 0),
    ki_krit_nevner = c(1, 1, 1, 0, 0, 0)
  ) %>%
    group_by(sykehus)

  svar_ugruppert = tibble(
    est = NA_real_,
    ki_teller = NA_integer_, ki_nevner = NA_integer_,
    konfint_nedre = NA_real_, konfint_ovre = NA_real_
  )

  svar_gruppert = tibble(
    sykehus = factor(c("A", "B")), est = c(0, NA_real_),
    ki_teller = c(0L, NA_integer_), ki_nevner = c(0L, NA_integer_),
    konfint_nedre = c(0, NA_real_), konfint_ovre = c(binom::binom.wilson(0, 3)$upper, NA_real_)
  )

  expect_identical(aggreger_ki_prop(d_ugruppert), svar_ugruppert)
  expect_identical(aggreger_ki_prop(d_gruppert), svar_gruppert)
})


# 1) Hvordan skal funksjonen håndtere missing i grupperingsvariabel?
#
# 2) Hvordan håndtere grupperingsvariabel er faktor som har nivå som ikke eksisterer i datasettet?
# Eks.:
# d_gruppert$sykehus = factor(d_gruppert$sykehus, levels = LETTERS[1:4])
# d_gruppert %>% group_by(sykehus, .drop = FALSE) %>% summarise(snitt=mean(ki_krit_teller))
#   Her skal NaN bli til NA i tilsvarende aggreger_ki_prop()-kjøring
test_that("Funksjonen gir en advarsel når det finnes NA-verdier i grupperingsvariabel", {
  n = 1000
  d = lag_d(n)
  d$gruppe[1] = NA
  expect_warning(aggreger_ki_prop(d, gruppering = "gruppe"))
})

# Hvordan skal funksjonen håndtere tilfeller hvor ingen er i en av gruppene?
# Vil her at vi får ut en oppføring for *HVER* gruppe, inkludert de som ikke har noen observasjoner
# De må da bli 0 for alle variabler.
# Grupper som ikke har noen verdier kan eventuelt filtreres ut i etterkant hvis de ikke skal være med i figurer, etc.
test_that("Funksjonen returnerer verdier for alle grupper i inndata, selv de gruppene som ikke inneholder observasjoner", {
  n = 1000
  d = lag_d(n)
  expect_equal(nrow(aggreger_ki_prop(d, gruppering = "gruppe")), length(levels(d$gruppe)))
})


# Test datasett:
# https://stackoverflow.com/questions/42734547/generating-random-strings
